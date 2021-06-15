{-# LANGUAGE DataKinds #-}

-- | Vulkan Swapchain extension
module Popcorn.Engine.Renderer.Vulkan.Swapchain
    ( -- * Data types
      Swapchain(..)

      -- * Swapchain creation/destruction
    , withSwapchain

      -- * Presentation
    , acquireSwapchainImage
    , queueImageForPresentation
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Managed (MonadManaged)
import Data.Bits (Bits((.&.), (.|.), zeroBits))
import Data.Word (Word32)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk
import qualified Vulkan.Zero as Vk

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Common.Utils (maybeToEither)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.Image
    ( Image(Image)
    , canCreateImage
    )
import Popcorn.Engine.Renderer.Vulkan.Internal.Surface (Surface(..), SurfaceInfo(..))
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( QueueFamily
    , VulkanDevice
    , queueFamilyIndex
    )
import Popcorn.Engine.Settings (Settings(..), VerticalSyncMode(..))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)

-- | Vulkan Swapchain and associated data
data Swapchain = Swapchain
    { swapchainHandle :: Vk.SwapchainKHR
    , swapchainImage :: Image
    }

-- | Manages a Swapchain
withSwapchain
    :: MonadManaged m
    => VulkanDevice
    -> Vk.Device
    -> Surface
    -> Settings
    -> QueueFamily
    -> QueueFamily
    -> m Swapchain
withSwapchain vd device surface settings graphicsQueue presentQueue = do
    bracketManaged
        (createSwapchain vd device surface settings graphicsQueue presentQueue)
        (destroySwapchain device)

createSwapchain
    :: VulkanDevice
    -> Vk.Device
    -> Surface
    -> Settings
    -> QueueFamily
    -> QueueFamily
    -> IO Swapchain
createSwapchain vd device surface settings graphicsQueue presentQueue = do
    canCreateSwapchain vd (surfaceInfo surface) >>= \case
        Left err -> throwIO
            (EngineException ("[Vulkan] Cannot create swapchain. " <> err))
        Right _ -> pure ()

    swapchainHandle <- Vk.createSwapchainKHR device
        (mkSwapchainCreateInfoKHR surface settings graphicsQueue presentQueue) Nothing

    (vkResult, images) <- Vk.getSwapchainImagesKHR device swapchainHandle

    -- Could this only happen in the unlikely case there is a bug in vk bindings/driver?
    when (vkResult /= Vk.SUCCESS) $
        throwIO (EngineException "[Renderer] Could not retrieve all swapchain images!")

    -- For now, only 1 swapchain image is supported to minimize synchronization code
    when (V.length images /= 1) $ do
        throwIO (EngineException "[Renderer] Only 1 swapchain image is supported for now!")

    engineLog ("Vulkan Swapchain created with " <> T.pack (show (V.length images)) <> " images")

    let imageFormat = fst (selectSurfaceFormat surface)
        swapchainImage = Image (V.head images) imageFormat 

    return Swapchain{..}

destroySwapchain :: Vk.Device -> Swapchain -> IO ()
destroySwapchain device Swapchain{..} =
    Vk.destroySwapchainKHR device swapchainHandle Nothing

mkSwapchainCreateInfoKHR
    :: Surface
    -> Settings
    -> QueueFamily
    -> QueueFamily
    -> Vk.SwapchainCreateInfoKHR '[]
mkSwapchainCreateInfoKHR surface settings graphicsQueue presentQueue = Vk.zero
    { Vk.surface = surfaceHandle surface
    , Vk.minImageCount = 1
    , Vk.imageFormat = imageFormat
    , Vk.imageColorSpace = imageColorSpace
    , Vk.imageExtent = swapchainExtent (surfaceInfo surface)
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
    surfaceCapabilities = surfaceInfoCapabilities (surfaceInfo surface)
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

-- | Acquire a Swapchain image for presentation and return its index. Semaphore should be
-- be signaled before the image can be used.
acquireSwapchainImage :: Vk.Device -> Swapchain -> Vk.Semaphore -> IO Word32
acquireSwapchainImage device swapchain semaphore = do
    let timeout = maxBound

    (vkResult, imageIndex) <- Vk.acquireNextImageKHR device (swapchainHandle swapchain)
        timeout semaphore Vk.NULL_HANDLE

    when (vkResult /= Vk.SUCCESS) $ do
        throwIO $ EngineException (T.pack ("Acquire image failed: " <> show vkResult))

    pure imageIndex

-- | Queues an acquired image for presentation.
queueImageForPresentation :: Vk.Queue -> Swapchain -> Word32 -> Vk.Semaphore -> IO () 
queueImageForPresentation queueHandle swapchain imageIndex semaphore = do
    vkResult <- Vk.queuePresentKHR queueHandle
        (mkPresentInfoKHR swapchain imageIndex semaphore)

    when (vkResult /= Vk.SUCCESS) $ do
        throwIO $ EngineException (T.pack ("Queue present failed: " <> show vkResult))

mkPresentInfoKHR :: Swapchain -> Word32 -> Vk.Semaphore -> Vk.PresentInfoKHR '[] 
mkPresentInfoKHR swapchain imageIndex semaphore = Vk.zero
    { Vk.waitSemaphores = [semaphore]
    , Vk.swapchains = [swapchainHandle swapchain]
    , Vk.imageIndices = [imageIndex]
    }