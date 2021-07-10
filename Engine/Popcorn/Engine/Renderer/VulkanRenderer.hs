{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | The Vulkan Renderer. Use it qualified as R.
module Popcorn.Engine.Renderer.VulkanRenderer
    ( -- * Initialization
      withVulkanRenderer
    , withVulkanRenderContext
    , withVulkanRendererStatic

    -- * Renderization
    , frameVulkan
  
    -- * Re-exports
    , module Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ) where

import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged)
import Data.Maybe (fromJust)
import Data.Word (Word32)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Platform.GLFW.Internal.Window (Window)
import Popcorn.Engine.Renderer.Vulkan.CommandBuffer
    ( recordSingleUseCommandBuffer
    , resetCommandPool
    , withCommandBuffers
    , withCommandPool
    )
import Popcorn.Engine.Renderer.Vulkan.Framebuffer (Attachments(..), withFramebuffer)
import Popcorn.Engine.Renderer.Vulkan.Image (Image(..), mkImageSubresourceRangeWholeImage)
import Popcorn.Engine.Renderer.Vulkan.Instance (withInstance)
import Popcorn.Engine.Renderer.Vulkan.Internal.Surface
    ( querySurfaceInfo
    , surfaceHandle
    , surfaceInfoCapabilities
    )
import Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ( Renderer(..)
    , VulkanFrameStatus(..)
    , VulkanRendererInteractive(..)
    , VulkanRendererStatic(..)
    , VulkanRenderContext(..)
    , VulkanSwapchain(..)
    )
import Popcorn.Engine.Renderer.Vulkan.LogicalDevice (withLogicalDevice)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( QueueFamily(..)
    , findGraphicsQueueFamilyIndex
    , selectInteractiveGraphicsDevice
    , selectGraphicsDevice
    , vulkanDeviceFriendlyDesc
    , vdPhysicalDevice
    )
import Popcorn.Engine.Renderer.Vulkan.Pipeline (PipelineDef(..), withPipelines)
import Popcorn.Engine.Renderer.Vulkan.Platform.GLFW (vulkanRequiredInstanceExtensions)
import Popcorn.Engine.Renderer.Vulkan.RenderPass (useRenderPass, withRenderPass)
import Popcorn.Engine.Renderer.Vulkan.Shader (loadShaderBytecode, withShaderModule)
import Popcorn.Engine.Renderer.Vulkan.Surface
    ( withVulkanSurface
    , findPresentQueueFamilyIndex
    )
import Popcorn.Engine.Renderer.Vulkan.Swapchain
    ( acquireSwapchainImage
    , queueImageForPresentation
    , withSwapchain
    )
import Popcorn.Engine.Renderer.Vulkan.Synchronization (waitDeviceIdle, withSemaphore)
import Popcorn.Engine.Renderer.Vulkan.Utils (formatList)
import Popcorn.Engine.Settings (Settings(..))

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk
import qualified Vulkan.Zero as Vk

-- | Initializes and manages a Vulkan renderer to render interactive graphics
withVulkanRenderer
    :: MonadManaged m
    => Application                                  -- ^ Application properties
    -> Window                                       -- ^ Window
    -> m (Either T.Text VulkanRendererInteractive)  -- ^ The Vulkan Renderer
withVulkanRenderer app window = runExceptT $ do
    instExts <- liftIO vulkanRequiredInstanceExtensions

    rInstance <- withInstance app instExts

    liftIO (engineLog (formatList "Vulkan instance extensions" instExts))

    rGraphicsDevice <- ExceptT (liftIO (selectInteractiveGraphicsDevice rInstance))

    rSurface <- withVulkanSurface window rInstance

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

    rCommandPool <- withCommandPool rLogicalDevice rGraphicsQueue
    rCommandBuffer <- V.head <$> withCommandBuffers rLogicalDevice rCommandPool 1

    rImageReadySemaphore <- withSemaphore rLogicalDevice
    rCommandsExecutedSemaphore <- withSemaphore rLogicalDevice

    liftIO (engineLog (vulkanDeviceFriendlyDesc rGraphicsDevice)
        >>  engineLog "Initialized Renderer: Vulkan 1.0")

    pure VulkanRendererInteractive{..}

-- | Initializes and manages a Vulkan render context to render interactive graphics
withVulkanRenderContext
    :: MonadManaged m
    => VulkanRendererInteractive    -- ^ The Vulkan Renderer to which associate a context
    -> Settings                     -- ^ Engine dynamic settings
    -> m VulkanRenderContext        -- ^ The Vulkan Render Context
withVulkanRenderContext VulkanRendererInteractive{..} settings = do
    let rcRenderer = VulkanRendererInteractive{..}

    surfaceInfo <- liftIO $ querySurfaceInfo (vdPhysicalDevice rGraphicsDevice)
        (surfaceHandle rSurface)

    let Vk.Extent2D width height = Vk.currentExtent (surfaceInfoCapabilities surfaceInfo)

    rcSwapchain <- withSwapchain rGraphicsDevice rLogicalDevice
        rSurface surfaceInfo settings rGraphicsQueue rPresentQueue

    let imageView = fromJust (swapchainImageView rcSwapchain)
        attachments = Attachments imageView (Vk.Extent2D width height)

    rcRenderPass <- withRenderPass rLogicalDevice (swapchainImageFormat rcSwapchain) False
    rcRenderPassClearScreen <- withRenderPass rLogicalDevice
        (swapchainImageFormat rcSwapchain) True
    rcFramebuffer <- withFramebuffer rLogicalDevice rcRenderPass attachments

    vsBytes <- liftIO $ loadShaderBytecode "SimpleVS"
    fsBytes <- liftIO $ loadShaderBytecode "SimpleFS"

    vs <- withShaderModule rLogicalDevice "SimpleVS" vsBytes
    fs <- withShaderModule rLogicalDevice "SimpleFS" fsBytes

    let pipelineDefs = [PipelineDef rcRenderPass 0 vs fs
            (Vk.Offset2D 0 0) (Vk.Extent2D width height)]

    rcGraphicsPipelines <- withPipelines rLogicalDevice pipelineDefs

    liftIO (engineLog ("Initialized Render Context with " <> T.pack (show settings)))

    pure VulkanRenderContext{..}

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

-- | Render a triangle (with a render pass, with clear screen, and vertex data in shader code)
frameVulkan :: VulkanRenderContext -> IO VulkanFrameStatus
frameVulkan renderContext = do
    acquireSwapchainImage renderContext >>= \case
        Left _ -> pure VulkanFrameStatusRecreateSwapchain
        Right imageIndex -> do
            prepareRenderWork6 renderContext
            submitRenderWork renderContext imageIndex

-- Prepare the frame render work (without clear screen)
prepareRenderWork1 :: VulkanRenderContext -> IO ()
prepareRenderWork1 VulkanRenderContext{..} = do
    let VulkanRendererInteractive{..} = rcRenderer

    recordSingleUseCommandBuffer rCommandBuffer $ do
        let imageMemoryBarrier = Vk.SomeStruct Vk.zero
                { Vk.srcAccessMask = Vk.zero
                , Vk.dstAccessMask = Vk.ACCESS_MEMORY_READ_BIT  
                , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
                , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = imageHandle (swapchainImage rcSwapchain)
                , Vk.subresourceRange =
                    mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT
                }

        Vk.cmdPipelineBarrier rCommandBuffer
            Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT Vk.PIPELINE_STAGE_TRANSFER_BIT
            Vk.zero V.empty V.empty [imageMemoryBarrier]

-- Prepare the frame render work (with clear screen)
prepareRenderWork2 :: VulkanRenderContext -> IO ()
prepareRenderWork2 VulkanRenderContext{..} = do
    let VulkanRendererInteractive{..} = rcRenderer

    recordSingleUseCommandBuffer rCommandBuffer $ do
        let imageMemoryBarrier = Vk.SomeStruct Vk.zero
                { Vk.srcAccessMask = Vk.zero
                , Vk.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT  
                , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
                , Vk.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = imageHandle (swapchainImage rcSwapchain)
                , Vk.subresourceRange =
                    mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT
                }

        Vk.cmdPipelineBarrier rCommandBuffer
            Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.PIPELINE_STAGE_TRANSFER_BIT
            Vk.zero V.empty V.empty [imageMemoryBarrier]

        let clearColor = Vk.Float32 0.66 0.33 0.0 0.0
            range = mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT

        Vk.cmdClearColorImage rCommandBuffer (imageHandle (swapchainImage rcSwapchain))
            Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL clearColor [range]

        let imageMemoryBarrier2 = Vk.SomeStruct Vk.zero
                { Vk.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT
                , Vk.dstAccessMask = Vk.ACCESS_MEMORY_READ_BIT  
                , Vk.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = imageHandle (swapchainImage rcSwapchain)
                , Vk.subresourceRange =
                    mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT
                }

        Vk.cmdPipelineBarrier rCommandBuffer
            Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
            Vk.zero V.empty V.empty [imageMemoryBarrier2]

-- Prepare the frame render work (with a render pass, and without clear screen)
prepareRenderWork3 :: VulkanRenderContext -> IO ()
prepareRenderWork3 VulkanRenderContext{..} = do
    let VulkanRendererInteractive{..} = rcRenderer

    recordSingleUseCommandBuffer rCommandBuffer $
        useRenderPass rcRenderPass rCommandBuffer rcFramebuffer Nothing $
            pure ()

-- Prepare the frame render work (with a render pass and with clear screen)
prepareRenderWork4 :: VulkanRenderContext -> IO ()
prepareRenderWork4 VulkanRenderContext{..} = do
    let clearColor = Vk.Color (Vk.Float32 0.66 0.33 0.0 0.0)
        VulkanRendererInteractive{..} = rcRenderer

    recordSingleUseCommandBuffer rCommandBuffer $ do
        useRenderPass rcRenderPassClearScreen rCommandBuffer rcFramebuffer
            (Just clearColor) $ pure ()

-- Prepare the frame render work (with a render pass, without clear screen, and vertex data in shader code)
prepareRenderWork5 :: VulkanRenderContext -> IO ()
prepareRenderWork5 VulkanRenderContext{..} = do
    let VulkanRendererInteractive{..} = rcRenderer

    recordSingleUseCommandBuffer rCommandBuffer $
        useRenderPass rcRenderPass rCommandBuffer rcFramebuffer Nothing $ do
            Vk.cmdBindPipeline rCommandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS
                (V.head rcGraphicsPipelines)
            Vk.cmdDraw rCommandBuffer 3 1 0 0

-- Prepare the frame render work (with a render pass, with clear screen, and vertex data in shader code)
prepareRenderWork6 :: VulkanRenderContext -> IO ()
prepareRenderWork6 VulkanRenderContext{..} = do
    let clearColor = Vk.Color (Vk.Float32 0.66 0.33 0.0 0.0)
        VulkanRendererInteractive{..} = rcRenderer

    recordSingleUseCommandBuffer rCommandBuffer $
        useRenderPass rcRenderPassClearScreen rCommandBuffer rcFramebuffer
            (Just clearColor) $ do
                Vk.cmdBindPipeline rCommandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS
                    (V.head rcGraphicsPipelines)
                Vk.cmdDraw rCommandBuffer 3 1 0 0

-- Submit the render work and starts the presentation process
submitRenderWork :: VulkanRenderContext -> Word32 -> IO VulkanFrameStatus
submitRenderWork renderContext imageIndex = do
    let VulkanRendererInteractive{..} = rcRenderer renderContext
        submitInfo = Vk.SomeStruct Vk.zero
            { Vk.waitSemaphores = [rImageReadySemaphore]
            , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , Vk.commandBuffers = [Vk.commandBufferHandle rCommandBuffer]
            , Vk.signalSemaphores = [rCommandsExecutedSemaphore]
            }
        queueHandle = queueFamilyHandle rGraphicsQueue 

    Vk.queueSubmit queueHandle [submitInfo] Vk.NULL_HANDLE 

    queueImageForPresentation renderContext imageIndex >>= \case
        Left  _  -> pure VulkanFrameStatusRecreateSwapchain
        Right () -> do
            waitDeviceIdle rLogicalDevice
            resetCommandPool rLogicalDevice rCommandPool
            pure VulkanFrameStatusOK