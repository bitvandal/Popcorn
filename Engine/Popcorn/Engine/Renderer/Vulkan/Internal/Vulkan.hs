module Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ( -- * Types
      Renderer(..)
    , VulkanRendererInteractive(..)
    , VulkanRendererStatic(..)
    ) where

import Popcorn.Engine.Renderer.Vulkan.Framebuffer (Framebuffer)
import Popcorn.Engine.Renderer.Vulkan.Internal.Surface (Surface)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (VulkanDevice, QueueFamily)
import Popcorn.Engine.Renderer.Vulkan.Swapchain (Swapchain)

import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk

-- | The Vulkan renderers: interactive, static.
data Renderer
    = RendererInteractive VulkanRendererInteractive
    | RendererStatic VulkanRendererStatic

-- | Interactive renderer
data VulkanRendererInteractive = VulkanRendererInteractive
    { rInstance :: Vk.Instance              -- Vulkan instance
    , rGraphicsDevice :: VulkanDevice       -- A Vulkan Device with Graphics capabilities
    , rLogicalDevice :: Vk.Device           -- Vulkan logical device
    , rGraphicsQueue :: QueueFamily         -- Queue family for graphics and transfer ops
    , rCommandPool :: Vk.CommandPool        -- Command Pool (Graphics queue family)
    , rCommandBuffer :: Vk.CommandBuffer    -- Command Buffer
    , rPresentQueue :: QueueFamily          -- Queue family for presentation ops
    , rSurface :: Surface                   -- Surface
    , rSwapchain :: Swapchain               -- Swapchain
    , rImageReadySemaphore :: Vk.Semaphore  -- Swapchain image is ready 
    , rCommandsExecutedSemaphore :: Vk.Semaphore   -- Command Buffer commands are executed
    , rRenderPass :: Vk.RenderPass          -- Vulkan Render Pass (static)
    , rRenderPassClearScreen :: Vk.RenderPass   -- Vulkan Render Pass (clears screen)
    , rFramebuffer :: Framebuffer           -- Wrapper over Vulkan Framebuffer
    , rGraphicsPipelines :: V.Vector Vk.Pipeline -- Graphics pipelines
    }

-- | Renders static images  
data VulkanRendererStatic = VulkanRendererStatic
    { rInstance :: Vk.Instance              -- Vulkan instance
    , rGraphicsDevice :: VulkanDevice       -- A Vulkan Device with Graphics capabilities
    , rLogicalDevice :: Vk.Device           -- Vulkan logical device
    , rGraphicsQueue :: QueueFamily         -- Queue family for graphics and transfer ops
    }