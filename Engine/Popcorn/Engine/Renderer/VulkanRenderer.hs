-- | The Vulkan Renderer. Use it qualified as R.
module Popcorn.Engine.Renderer.VulkanRenderer
    ( -- * Initialization
      withVulkanRenderer
    , withVulkanRendererStatic

    -- * Renderization
    , frame
    , frame2
    , frame3
    , frame4

    -- * Re-exports
    , module Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ) where

import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged)
import Data.Maybe (fromJust)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Platform.GLFW.Window (Window)
import Popcorn.Engine.Renderer.Vulkan.CommandBuffer
    ( recordSingleUseCommandBuffer
    , resetCommandPool
    , withCommandBuffers
    , withCommandPool
    )
import Popcorn.Engine.Renderer.Vulkan.Framebuffer
    ( Attachments(..)
    , withFramebuffer
    )
import Popcorn.Engine.Renderer.Vulkan.Image
    (Image(imageHandle)
    , mkImageSubresourceRangeWholeImage
    )
import Popcorn.Engine.Renderer.Vulkan.Instance (withInstance)
import Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ( Renderer(..)
    , VulkanRendererInteractive(..)
    , VulkanRendererStatic(..)
    )
import Popcorn.Engine.Renderer.Vulkan.LogicalDevice (withLogicalDevice)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( QueueFamily(..)
    , VulkanDevice(vdPhysicalDevice)
    , findGraphicsQueueFamilyIndex
    , selectInteractiveGraphicsDevice
    , selectGraphicsDevice
    , vulkanDeviceFriendlyDesc
    )
import Popcorn.Engine.Renderer.Vulkan.Platform.GLFW (vulkanRequiredInstanceExtensions)
import Popcorn.Engine.Renderer.Vulkan.RenderPass (useRenderPass, withRenderPass)
import Popcorn.Engine.Renderer.Vulkan.Surface
    ( withVulkanSurface
    , findPresentQueueFamilyIndex
    )
import Popcorn.Engine.Renderer.Vulkan.Swapchain
    ( acquireSwapchainImage
    , queueImageForPresentation
    , swapchainImage
    , swapchainImageFormat
    , swapchainImageView
    , withSwapchain
    )
import Popcorn.Engine.Renderer.Vulkan.Sychronization (waitDeviceIdle, withSemaphore)
import Popcorn.Engine.Renderer.Vulkan.Utils (formatList)
import Popcorn.Engine.Settings (Settings)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

-- | Initializes and manages a Vulkan renderer to render interactive graphics
withVulkanRenderer
    :: MonadManaged m
    => Application                                  -- ^ Application properties
    -> Window                                       -- ^ Window
    -> Settings                                     -- ^ Engine creation settings
    -> m (Either T.Text VulkanRendererInteractive)  -- ^ The Vulkan Renderer
withVulkanRenderer app window settings = runExceptT $ do
    instExts <- liftIO vulkanRequiredInstanceExtensions

    rInstance <- withInstance app instExts

    liftIO (engineLog (formatList "Vulkan instance extensions" instExts))

    rGraphicsDevice <- ExceptT (liftIO (selectInteractiveGraphicsDevice rInstance))

    rSurface <- withVulkanSurface window rInstance (vdPhysicalDevice rGraphicsDevice)

    graphicsQueueFamilyIndex <-
        ExceptT (pure (findGraphicsQueueFamilyIndex rGraphicsDevice))

    presentQueueFamilyIndex <- ExceptT (liftIO
        (findPresentQueueFamilyIndex rInstance rGraphicsDevice rSurface))

    rLogicalDevice <- withLogicalDevice rGraphicsDevice
        graphicsQueueFamilyIndex (Just presentQueueFamilyIndex)

    graphicsQueueHandle <- Vk.getDeviceQueue rLogicalDevice graphicsQueueFamilyIndex 0
    presentQueueHandle <- Vk.getDeviceQueue rLogicalDevice presentQueueFamilyIndex 0

    let rGraphicsQueue = QueueFamily graphicsQueueFamilyIndex graphicsQueueHandle 
        rPresentQueue  = QueueFamily presentQueueFamilyIndex presentQueueHandle

    rSwapchain <- withSwapchain rGraphicsDevice rLogicalDevice
        rSurface settings rGraphicsQueue rPresentQueue

    let imageView = fromJust (swapchainImageView rSwapchain)

    let width = fromIntegral (applicationMainWindowWidth app)
        height = fromIntegral (applicationMainWindowHeight app)
        attachments = Attachments imageView (Vk.Extent2D width height)

    rCommandPool <- withCommandPool rLogicalDevice rGraphicsQueue
    rCommandBuffer <- V.head <$> withCommandBuffers rLogicalDevice rCommandPool 1

    rImageReadySemaphore <- withSemaphore rLogicalDevice
    rCommandsExecutedSemaphore <- withSemaphore rLogicalDevice

    rRenderPass <- withRenderPass rLogicalDevice (swapchainImageFormat rSwapchain) False
    rRenderPassClearScreen <- withRenderPass rLogicalDevice
        (swapchainImageFormat rSwapchain) True
    rFramebuffer <- withFramebuffer rLogicalDevice rRenderPass attachments

    liftIO (engineLog (vulkanDeviceFriendlyDesc rGraphicsDevice)
        >>  engineLog "Initialized Renderer: Vulkan 1.0")

    pure VulkanRendererInteractive{..}

-- | Initializes and manages a Vulkan renderer to render to a static image
withVulkanRendererStatic
    :: MonadManaged m
    => Application                              -- ^ Application properties
    -> m (Either T.Text VulkanRendererStatic)   -- ^ The Vulkan Renderer
withVulkanRendererStatic app = runExceptT $ do
    rInstance <- withInstance app []

    rGraphicsDevice <- ExceptT (liftIO (selectGraphicsDevice rInstance))

    graphicsQueueFamilyIndex <-
        ExceptT (pure (findGraphicsQueueFamilyIndex rGraphicsDevice))

    rLogicalDevice <- withLogicalDevice
        rGraphicsDevice graphicsQueueFamilyIndex Nothing

    handle <- Vk.getDeviceQueue rLogicalDevice graphicsQueueFamilyIndex 0

    let rGraphicsQueue = QueueFamily graphicsQueueFamilyIndex handle

    liftIO (engineLog "Initialized Renderer static: Vulkan 1.0"
        >>  engineLog (vulkanDeviceFriendlyDesc rGraphicsDevice))

    pure VulkanRendererStatic{..}

-- | Render a frame (without clear screen)
frame :: VulkanRendererInteractive -> IO ()
frame VulkanRendererInteractive{..} = do
    imageIndex <- acquireSwapchainImage rLogicalDevice rSwapchain rImageReadySemaphore

    recordSingleUseCommandBuffer rCommandBuffer $ do
        let imageMemoryBarrier = Vk.SomeStruct Vk.zero
                { Vk.srcAccessMask = Vk.zero
                , Vk.dstAccessMask = Vk.ACCESS_MEMORY_READ_BIT  
                , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
                , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = imageHandle (swapchainImage rSwapchain)
                , Vk.subresourceRange =
                    mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT
                }

        Vk.cmdPipelineBarrier rCommandBuffer
            Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT Vk.PIPELINE_STAGE_TRANSFER_BIT
            Vk.zero V.empty V.empty [imageMemoryBarrier]

    let submitInfo = Vk.SomeStruct Vk.zero
            { Vk.waitSemaphores = [rImageReadySemaphore]
            , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , Vk.commandBuffers = [Vk.commandBufferHandle rCommandBuffer]
            , Vk.signalSemaphores = [rCommandsExecutedSemaphore]
            }
        queueHandle = queueFamilyHandle rGraphicsQueue 

    Vk.queueSubmit queueHandle [submitInfo] Vk.NULL_HANDLE 

    queueImageForPresentation (queueFamilyHandle rPresentQueue) rSwapchain imageIndex
        rCommandsExecutedSemaphore

    waitDeviceIdle rLogicalDevice

    resetCommandPool rLogicalDevice rCommandPool

-- | Render a frame (with clear screen)
frame2 :: VulkanRendererInteractive -> IO ()
frame2 VulkanRendererInteractive{..} = do
    imageIndex <- acquireSwapchainImage rLogicalDevice rSwapchain rImageReadySemaphore

    recordSingleUseCommandBuffer rCommandBuffer $ do
        let imageMemoryBarrier = Vk.SomeStruct Vk.zero
                { Vk.srcAccessMask = Vk.zero
                , Vk.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT  
                , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
                , Vk.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = imageHandle (swapchainImage rSwapchain)
                , Vk.subresourceRange =
                    mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT
                }

        Vk.cmdPipelineBarrier rCommandBuffer
            Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.PIPELINE_STAGE_TRANSFER_BIT
            Vk.zero V.empty V.empty [imageMemoryBarrier]

        let clearColor = Vk.Float32 0.66 0.33 0.0 0.0
            range = mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT

        Vk.cmdClearColorImage rCommandBuffer (imageHandle (swapchainImage rSwapchain))
            Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL clearColor [range]

        let imageMemoryBarrier2 = Vk.SomeStruct Vk.zero
                { Vk.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT
                , Vk.dstAccessMask = Vk.ACCESS_MEMORY_READ_BIT  
                , Vk.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = imageHandle (swapchainImage rSwapchain)
                , Vk.subresourceRange =
                    mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT
                }

        Vk.cmdPipelineBarrier rCommandBuffer
            Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
            Vk.zero V.empty V.empty [imageMemoryBarrier2]

    let submitInfo =  Vk.SomeStruct Vk.zero
            { Vk.waitSemaphores = [rImageReadySemaphore]
            , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , Vk.commandBuffers = [Vk.commandBufferHandle rCommandBuffer]
            , Vk.signalSemaphores = [rCommandsExecutedSemaphore]
            }
        queueHandle = queueFamilyHandle rGraphicsQueue 

    Vk.queueSubmit queueHandle [submitInfo] Vk.NULL_HANDLE 

    queueImageForPresentation (queueFamilyHandle rPresentQueue) rSwapchain imageIndex
        rCommandsExecutedSemaphore

    waitDeviceIdle rLogicalDevice

    resetCommandPool rLogicalDevice rCommandPool

-- | Render a frame (with a render pass, and without clear screen)
frame3 :: VulkanRendererInteractive -> IO ()
frame3 VulkanRendererInteractive{..} = do
    imageIndex <- acquireSwapchainImage rLogicalDevice rSwapchain rImageReadySemaphore

    recordSingleUseCommandBuffer rCommandBuffer $
        useRenderPass rRenderPass rCommandBuffer rFramebuffer Nothing $
            pure ()

    let submitInfo = Vk.SomeStruct Vk.zero
            { Vk.waitSemaphores = [rImageReadySemaphore]
            , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , Vk.commandBuffers = [Vk.commandBufferHandle rCommandBuffer]
            , Vk.signalSemaphores = [rCommandsExecutedSemaphore]
            }
        queueHandle = queueFamilyHandle rGraphicsQueue 

    Vk.queueSubmit queueHandle [submitInfo] Vk.NULL_HANDLE 

    queueImageForPresentation (queueFamilyHandle rPresentQueue) rSwapchain imageIndex
        rCommandsExecutedSemaphore

    waitDeviceIdle rLogicalDevice

    resetCommandPool rLogicalDevice rCommandPool

-- | Render a frame (with a render pass and with clear screen)
frame4 :: VulkanRendererInteractive -> IO ()
frame4 VulkanRendererInteractive{..} = do
    imageIndex <- acquireSwapchainImage rLogicalDevice rSwapchain rImageReadySemaphore

    let clearColor = Vk.Color (Vk.Float32 0.66 0.33 0.0 0.0)

    recordSingleUseCommandBuffer rCommandBuffer $ do
        useRenderPass rRenderPassClearScreen rCommandBuffer rFramebuffer
            (Just clearColor) $ pure ()

    let submitInfo = Vk.SomeStruct Vk.zero
            { Vk.waitSemaphores = [rImageReadySemaphore]
            , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , Vk.commandBuffers = [Vk.commandBufferHandle rCommandBuffer]
            , Vk.signalSemaphores = [rCommandsExecutedSemaphore]
            }
        queueHandle = queueFamilyHandle rGraphicsQueue 

    Vk.queueSubmit queueHandle [submitInfo] Vk.NULL_HANDLE 

    queueImageForPresentation (queueFamilyHandle rPresentQueue) rSwapchain imageIndex
        rCommandsExecutedSemaphore

    waitDeviceIdle rLogicalDevice

    resetCommandPool rLogicalDevice rCommandPool