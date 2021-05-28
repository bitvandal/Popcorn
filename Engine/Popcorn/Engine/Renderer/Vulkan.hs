-- | The Vulkan Renderer
module Popcorn.Engine.Renderer.Vulkan
    ( -- * Initialization
      withVulkanRenderer
    , withVulkanRendererOffscreen

    -- * Re-exports
    , module Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ) where

import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Platform.GLFW.Window (Window)
import Popcorn.Engine.Renderer.Vulkan.Instance (withInstance)
import Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan (Renderer(..))
import Popcorn.Engine.Renderer.Vulkan.LogicalDevice (withLogicalDevice)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( QueueFamily(queueFamilyIndex, queueFamilyHandles)
    , VulkanDevice(vdPhysicalDevice)
    , findGraphicsQueueFamily
    , selectGraphicsDevice
    , selectOffscreenGraphicsDevice
    , vulkanDeviceFriendlyDesc
    )
import Popcorn.Engine.Renderer.Vulkan.Platform.GLFW (vulkanRequiredInstanceExtensions)
import Popcorn.Engine.Renderer.Vulkan.Surface (withVulkanSurface, findPresentQueueFamily)
import Popcorn.Engine.Renderer.Vulkan.Swapchain (withSwapchain)
import Popcorn.Engine.Renderer.Vulkan.Utils (formatList)
import Popcorn.Engine.Settings (Settings)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk

-- | Initializes and manages a Vulkan renderer to render interactive graphics
withVulkanRenderer
    :: MonadManaged m
    => Application                  -- ^ Application properties
    -> Window                       -- ^ Window
    -> Settings                     -- ^ Engine settings
    -> m (Either T.Text Renderer)   -- ^ The Vulkan Renderer
withVulkanRenderer app window settings = runExceptT $ do
    instExts <- liftIO vulkanRequiredInstanceExtensions

    rInstance <- withInstance app instExts

    liftIO (engineLog (formatList "Vulkan instance extensions" instExts))

    rGraphicsDevice <- ExceptT (liftIO (selectGraphicsDevice rInstance))

    surface <- withVulkanSurface window rInstance (vdPhysicalDevice rGraphicsDevice)

    graphicsQueue <- ExceptT (pure (findGraphicsQueueFamily rGraphicsDevice))
    presentQueue <- ExceptT (liftIO (findPresentQueueFamily rInstance rGraphicsDevice surface))

    rLogicalDevice <- withLogicalDevice rGraphicsDevice
        (queueFamilyIndex graphicsQueue) (Just (queueFamilyIndex presentQueue))

    graphicsQueueHandle <-
        Vk.getDeviceQueue rLogicalDevice (queueFamilyIndex graphicsQueue) 0

    presentQueueHandle <-
        Vk.getDeviceQueue rLogicalDevice (queueFamilyIndex presentQueue) 0

    swapchain <- withSwapchain rGraphicsDevice rLogicalDevice
        surface settings graphicsQueue presentQueue

    let rSurface = pure surface
        rGraphicsQueue = graphicsQueue { queueFamilyHandles = [graphicsQueueHandle] }
        rPresentQueue  = pure (presentQueue { queueFamilyHandles = [presentQueueHandle] })
        rSwapchain = pure swapchain

    liftIO (engineLog (vulkanDeviceFriendlyDesc rGraphicsDevice)
        >>  engineLog "Initialized Renderer: Vulkan 1.0")

    pure Renderer{..}

-- | Initializes and manages a Vulkan renderer to render off-screen
withVulkanRendererOffscreen
    :: MonadManaged m
    => Application                  -- ^ Application properties
    -> m (Either T.Text Renderer)   -- ^ The Vulkan Renderer
withVulkanRendererOffscreen app = runExceptT $ do
    rInstance <- withInstance app []

    rGraphicsDevice <- ExceptT (liftIO (selectOffscreenGraphicsDevice rInstance))

    graphicsQueue <- ExceptT (pure (findGraphicsQueueFamily rGraphicsDevice))

    rLogicalDevice <- withLogicalDevice
        rGraphicsDevice (queueFamilyIndex graphicsQueue) Nothing

    handle <- Vk.getDeviceQueue rLogicalDevice (queueFamilyIndex graphicsQueue) 0

    let rGraphicsQueue = graphicsQueue { queueFamilyHandles = V.singleton handle }
        rPresentQueue = Nothing
        rSurface = Nothing
        rSwapchain = Nothing

    liftIO (engineLog "Initialized off-screen renderer: Vulkan 1.0"
        >>  engineLog (vulkanDeviceFriendlyDesc rGraphicsDevice))

    pure Renderer{..}