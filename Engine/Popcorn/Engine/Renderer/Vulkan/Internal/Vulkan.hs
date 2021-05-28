module Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan
    ( -- * Types
      Renderer(..)
    ) where

import Popcorn.Engine.Renderer.Vulkan.Internal.Surface (Surface)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (VulkanDevice, QueueFamily)
import Popcorn.Engine.Renderer.Vulkan.Swapchain (Swapchain)

import qualified Vulkan.Core10 as Vk

-- | The Vulkan renderer
data Renderer = Renderer
    { rInstance :: Vk.Instance
    , rGraphicsDevice :: VulkanDevice       -- A Vulkan Device with Graphics capabilities
    , rLogicalDevice :: Vk.Device
    , rGraphicsQueue :: QueueFamily         -- Queue family for graphics and transfer ops
    , rPresentQueue :: Maybe QueueFamily    -- Queue family for presentation ops
    , rSurface :: Maybe Surface             -- Surface
    , rSwapchain :: Maybe Swapchain         -- Swapchain
    }