-- | Vulkan renderer internals
module Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ( -- * Types
      Renderer(..)
    , VulkanFrameStatus(..)
    , VulkanRendererInteractive(..)
    , VulkanRendererStatic(..)
    , VulkanRenderContext(..)
    , VulkanSwapchain(..)
    ) where

import Popcorn.Engine.Renderer.Vulkan.Framebuffer (Framebuffer)
import Popcorn.Engine.Renderer.Vulkan.Image (Image)
import Popcorn.Engine.Renderer.Vulkan.Internal.Surface (Surface)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (VulkanDevice, QueueFamily)
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk

-- | The Vulkan renderers: interactive, static.
data Renderer
    = RendererInteractive VulkanRendererInteractive
    | RendererStatic VulkanRendererStatic

-- | Interactive renderer
data VulkanRendererInteractive = VulkanRendererInteractive
    { -- | Vulkan instance
      rInstance :: Vk.Instance

      -- | A Vulkan Device with Graphics capabilities
    , rGraphicsDevice :: VulkanDevice

    -- | Vulkan logical device
    , rLogicalDevice :: Vk.Device

    -- | Queue family for graphics and transfer ops
    , rGraphicsQueue :: QueueFamily

    -- | Command Pool (Graphics queue family)
    , rCommandPool :: Vk.CommandPool

    -- | Command Buffer
    , rCommandBuffer :: Vk.CommandBuffer

    -- | Queue family for presentation ops
    , rPresentQueue :: QueueFamily

    -- | Surface
    , rSurface :: Surface

    -- | Swapchain image is ready
    , rImageReadySemaphore :: Vk.Semaphore 

    -- | Command Buffer commands are executed
    , rCommandsExecutedSemaphore :: Vk.Semaphore
    }

-- | Renders static images  
data VulkanRendererStatic = VulkanRendererStatic
    { -- | Vulkan instance
      rInstance :: Vk.Instance

      -- | A Vulkan Device with Graphics capabilities
    , rGraphicsDevice :: VulkanDevice

      -- | Vulkan logical device
    , rLogicalDevice :: Vk.Device

      -- | Queue family for graphics and transfer ops
    , rGraphicsQueue :: QueueFamily
    }

-- | Vulkan Render frame status
data VulkanFrameStatus
    = VulkanFrameStatusOK
    | VulkanFrameStatusRecreateSwapchain
    deriving stock (Eq, Show)

-- | The Vulkan render context
data VulkanRenderContext = VulkanRenderContext
    { -- | Interactive Renderer to which this context is bound to.
      rcRenderer :: VulkanRendererInteractive
 
      -- | Swapchain
    , rcSwapchain :: VulkanSwapchain            

      -- | Vulkan Render Pass 
    , rcRenderPass :: Vk.RenderPass

      -- | Vulkan Render Pass (clears screen)
    , rcRenderPassClearScreen :: Vk.RenderPass

      -- | Wrapper over Vulkan Framebuffer
    , rcFramebuffer :: Framebuffer

    -- | Graphics pipelines
    , rcGraphicsPipelines :: V.Vector Vk.Pipeline
    }

-- | The Vulkan Swapchain
data VulkanSwapchain = VulkanSwapchain
    { -- | Vulkan Swapchain handle
      swapchainHandle :: Vk.SwapchainKHR

      -- | The Swapchain image (for now 1)
    , swapchainImage :: Image

      -- | The Swapchain images format
    , swapchainImageFormat :: Vk.Format

      -- | The Swapchain image view handle for the Swapchain images
    , swapchainImageView :: Maybe Vk.ImageView
    }