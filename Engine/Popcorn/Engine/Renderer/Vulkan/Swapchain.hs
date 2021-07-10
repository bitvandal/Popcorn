{-# LANGUAGE DataKinds #-}

-- | Vulkan Swapchain extension
module Popcorn.Engine.Renderer.Vulkan.Swapchain
    ( -- * Swapchain creation/destruction
      withSwapchain

      -- * Presentation
    , acquireSwapchainImage
    , queueImageForPresentation
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Managed (MonadManaged)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.Bits (Bits((.&.), (.|.), zeroBits))
import Data.Word (Word32)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Common.Utils (maybeToEither)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.CommandBuffer (resetCommandPool)
import Popcorn.Engine.Renderer.Vulkan.Image (Image(Image), canCreateImage, withImageView)
import Popcorn.Engine.Renderer.Vulkan.Internal.Surface
    ( Surface(..)
    , SurfaceInfo(..)
    , surfaceHandle
    )
import Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ( VulkanRendererInteractive(..)
    , VulkanSwapchain(..)
    , VulkanRenderContext(..)
    , rcRenderer
    )
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (QueueFamily(..), VulkanDevice(..))
import Popcorn.Engine.Renderer.Vulkan.Synchronization
    ( unsignalSemaphoreSlow
    , waitDeviceIdle
    )
import Popcorn.Engine.Settings (Settings(..), VerticalSyncMode(..))

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk
import qualified Vulkan.Zero as Vk

-- | Manages a Swapchain
withSwapchain
    :: MonadManaged m
    => VulkanDevice
    -> Vk.Device
    -> Surface
    -> SurfaceInfo
    -> Settings
    -> QueueFamily
    -> QueueFamily
    -> m VulkanSwapchain
withSwapchain vd device surface surfaceInfo settings graphicsQueue presentQueue = do
    swapchain <- bracketManaged
        (createSwapchain vd device surface surfaceInfo settings graphicsQueue presentQueue)
        (destroySwapchain device)

    imageView <- withImageView device (swapchainImage swapchain)

    pure swapchain { swapchainImageView = Just imageView }

createSwapchain
    :: VulkanDevice
    -> Vk.Device
    -> Surface
    -> SurfaceInfo
    -> Settings
    -> QueueFamily
    -> QueueFamily
    -> IO VulkanSwapchain
createSwapchain vd device surface surfaceInfo settings graphicsQueue presentQueue = do
    canCreateSwapchain vd surfaceInfo >>= \case
        Left err -> throwIO
            (EngineException ("[Vulkan] Cannot create swapchain. " <> err))
        Right _ -> pure ()

    swapchainHandle <- Vk.createSwapchainKHR device (mkSwapchainCreateInfoKHR surface
        surfaceInfo settings graphicsQueue presentQueue) Nothing

    (vkResult, images) <- Vk.getSwapchainImagesKHR device swapchainHandle

    -- Could this only happen in the unlikely case there is a bug in vk bindings/driver?
    when (vkResult /= Vk.SUCCESS) $
        throwIO (EngineException "[Renderer] Could not retrieve all swapchain images!")

    -- For now, only 1 swapchain image is supported to minimize synchronization code
    when (V.length images /= 1) $ do
        throwIO (EngineException "[Renderer] Only 1 swapchain image is supported for now!")

    let swapchainImageFormat = fst (selectSurfaceFormat surface)
        swapchainImage = Image (V.head images) swapchainImageFormat
        swapchainImageView = Nothing

    engineLog ("Vulkan Swapchain created with " <> T.pack (show (V.length images)) <> " images")

    return VulkanSwapchain{..}

destroySwapchain :: Vk.Device -> VulkanSwapchain -> IO ()
destroySwapchain device VulkanSwapchain{..} =
    Vk.destroySwapchainKHR device swapchainHandle Nothing

mkSwapchainCreateInfoKHR
    :: Surface
    -> SurfaceInfo
    -> Settings
    -> QueueFamily
    -> QueueFamily
    -> Vk.SwapchainCreateInfoKHR '[]
mkSwapchainCreateInfoKHR surface surfaceInfo settings graphicsQueue presentQueue = Vk.zero
    { Vk.surface = surfaceHandle surface
    , Vk.minImageCount = 1
    , Vk.imageFormat = imageFormat
    , Vk.imageColorSpace = imageColorSpace
    , Vk.imageExtent = swapchainExtent surfaceInfo
    , Vk.imageArrayLayers = 1
    , Vk.imageUsage = swapchainImageUsage
    , Vk.imageSharingMode = sharingMode
    , Vk.queueFamilyIndices = queueFamilies
    , Vk.preTransform = transform
    , Vk.compositeAlpha = swapchainCompositeAlpha
    , Vk.presentMode = toVulkanPresentMode (sVerticalSync settings)
    , Vk.clipped = True
    , Vk.oldSwapchain = Vk.NULL_HANDLE
    }
  where
    surfaceCapabilities = surfaceInfoCapabilities surfaceInfo
    supportedTransforms = Vk.supportedTransforms surfaceCapabilities
    transform =
        if supportedTransforms .&. Vk.SURFACE_TRANSFORM_IDENTITY_BIT_KHR /= zeroBits
            then Vk.SURFACE_TRANSFORM_IDENTITY_BIT_KHR
            else Vk.currentTransform surfaceCapabilities
    (sharingMode, queueFamilies) =
        if queueFamilyIndex graphicsQueue == queueFamilyIndex presentQueue
            then (Vk.SHARING_MODE_EXCLUSIVE, [])
            else (Vk.SHARING_MODE_CONCURRENT,
                  [queueFamilyIndex graphicsQueue, queueFamilyIndex presentQueue])
    (imageFormat, imageColorSpace) = selectSurfaceFormat surface

toVulkanPresentMode :: VerticalSyncMode -> Vk.PresentModeKHR
toVulkanPresentMode _ = Vk.PRESENT_MODE_FIFO_KHR

swapchainExtent :: SurfaceInfo -> Vk.Extent2D
swapchainExtent info = Vk.currentExtent (surfaceInfoCapabilities info)

selectSurfaceFormat :: Surface -> (Vk.Format, Vk.ColorSpaceKHR)
selectSurfaceFormat _ = preferredSurfaceFormat

preferredSurfaceFormat :: (Vk.Format, Vk.ColorSpaceKHR)
preferredSurfaceFormat = (preferredImageFormat, preferredImageColorSpace)

preferredImageFormat :: Vk.Format
preferredImageFormat = Vk.FORMAT_B8G8R8A8_SRGB

preferredImageColorSpace :: Vk.ColorSpaceKHR
preferredImageColorSpace = Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR

swapchainImageUsage :: Vk.ImageUsageFlagBits
swapchainImageUsage =
    Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT

swapchainCompositeAlpha :: Vk.CompositeAlphaFlagBitsKHR
swapchainCompositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR

-- Check if there is enough support to create the Swapchain with the desired configuration
canCreateSwapchain :: VulkanDevice -> SurfaceInfo -> IO (Either T.Text ())
canCreateSwapchain vd info = runExceptT $ do
    ExceptT (pure (swapchainSupportsFormat info preferredSurfaceFormat
        >> surfaceHasValidExtent info
        >> surfaceSupportsImageUsage info
        >> surfaceSupportsCompositeAlpha info))

    ExceptT (canCreateImage vd preferredImageFormat Vk.IMAGE_TYPE_2D 
        Vk.IMAGE_TILING_OPTIMAL swapchainImageUsage Vk.zero
        (swapchainExtent info))

swapchainSupportsFormat
    :: SurfaceInfo
    -> (Vk.Format, Vk.ColorSpaceKHR)
    -> Either T.Text ()
swapchainSupportsFormat info (desiredFormat, desiredColorSpace) =
    maybeToEither "Surface format not supported" found
  where
    formats = surfaceInfoFormats info
    found = () <$ V.find (== Vk.SurfaceFormatKHR desiredFormat desiredColorSpace) formats

surfaceHasValidExtent :: SurfaceInfo -> Either T.Text ()
surfaceHasValidExtent info = 
    if Vk.currentExtent (surfaceInfoCapabilities info) == Vk.Extent2D 0 0
    then Left "Surface extent cannot be (0, 0). Is the window minimized?"
    else Right ()

surfaceSupportsImageUsage :: SurfaceInfo -> Either T.Text ()
surfaceSupportsImageUsage info =
    if supportedUsageFlags .&. swapchainImageUsage == zeroBits 
    then Left "Surface does not support required usage flags."
    else Right ()
  where
    supportedUsageFlags = Vk.supportedUsageFlags (surfaceInfoCapabilities info)

surfaceSupportsCompositeAlpha :: SurfaceInfo -> Either T.Text ()
surfaceSupportsCompositeAlpha info =
    if supportedCompositeAlpha .&. swapchainCompositeAlpha == zeroBits 
    then Left "Surface does not support required composite alpha."
    else Right ()
  where
    supportedCompositeAlpha = Vk.supportedCompositeAlpha (surfaceInfoCapabilities info)

-- | Acquires a Swapchain image for presentation and return its index.
--
-- Semaphore should be signaled before the image can be used. In case Vk_SUBOPTIMAL_KHR
-- status is received, this function will unsignal the 'Image Ready' semaphore in the
-- renderer.
acquireSwapchainImage
    :: VulkanRenderContext          -- ^ The Vulkan Render Context
    -> IO (Either Vk.Result Word32) -- ^ Returns the acquired image index, or in case
                                    -- of partial success, returns the specific vkResult
acquireSwapchainImage VulkanRenderContext{..} = do
    let timeout = maxBound
        VulkanRendererInteractive{..} = rcRenderer

    (vkResult, imageIndex) <- Vk.acquireNextImageKHR rLogicalDevice
        (swapchainHandle rcSwapchain) timeout rImageReadySemaphore Vk.NULL_HANDLE

    case vkResult of
        Vk.SUCCESS -> pure (Right imageIndex)
        Vk.SUBOPTIMAL_KHR -> do
            engineLog ("Acquire image partial success: " <> T.pack (show vkResult))
            unsignalSemaphoreSlow VulkanRendererInteractive{..} rImageReadySemaphore
            pure (Left vkResult)
        _ -> throwIO $ EngineException (T.pack ("Acquire image error: " <> show vkResult))

-- | Queues an acquired image for presentation.
queueImageForPresentation
    :: VulkanRenderContext          -- ^ The Vulkan Render Context
    -> Word32                       -- ^ Index of the swapchain image to queue for present
    -> IO (Either Vk.Result ())     -- ^ In case of partial success, returns the specific
                                    -- vkResult
queueImageForPresentation VulkanRenderContext{..} imageIndex = do
    let VulkanRendererInteractive{..} = rcRenderer

    vkResult <- Vk.queuePresentKHR (queueFamilyHandle rPresentQueue)
        (mkPresentInfoKHR rcSwapchain imageIndex rCommandsExecutedSemaphore)

    case vkResult of
        Vk.SUCCESS -> pure (Right ())
        Vk.SUBOPTIMAL_KHR -> do
            engineLog ("Queue present partial success: " <> T.pack (show vkResult))
            waitDeviceIdle rLogicalDevice
            resetCommandPool rLogicalDevice rCommandPool
            pure (Left vkResult)
        _ -> throwIO $ EngineException (T.pack ("Queue present error: " <> show vkResult))

mkPresentInfoKHR :: VulkanSwapchain -> Word32 -> Vk.Semaphore -> Vk.PresentInfoKHR '[] 
mkPresentInfoKHR swapchain imageIndex semaphore = Vk.zero
    { Vk.waitSemaphores = [semaphore]
    , Vk.swapchains = [swapchainHandle swapchain]
    , Vk.imageIndices = [imageIndex]
    }