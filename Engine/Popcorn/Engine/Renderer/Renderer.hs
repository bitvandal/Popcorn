-- | API agnostic renderer
module Popcorn.Engine.Renderer.Renderer
    ( -- * Data types
      FrameStatus(..)

      -- * Rendering
    , frameRender
    ) where

import Popcorn.Engine.Renderer.VulkanRenderer
    ( VulkanFrameStatus(..)
    , VulkanRenderContext
    , frameVulkan
    )

-- | Render frame status
data FrameStatus
    = FrameStatusOK
    | FrameStatusRenderContextInvalid
    deriving stock (Eq, Show)

-- | Entry point for rendering a frame
frameRender :: VulkanRenderContext -> IO FrameStatus
frameRender renderContext = frameVulkan renderContext >>= \case
    VulkanFrameStatusOK -> pure FrameStatusOK
    VulkanFrameStatusRecreateSwapchain -> pure FrameStatusRenderContextInvalid