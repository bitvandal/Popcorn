{-# LANGUAGE DataKinds #-}

-- | Vulkan Framebuffer
module Popcorn.Engine.Renderer.Vulkan.Framebuffer
    ( -- * Data types
      Attachments(..)
    , Framebuffer(..)

      -- * Resource creation/release
    , withFramebuffer
    ) where

import Control.Monad.Managed (MonadManaged)
import Popcorn.Engine.Managed.Extra (bracketManaged)

import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Zero as Vk

-- | Framebuffer
data Framebuffer = Framebuffer
    { framebufferHandle :: Vk.Framebuffer
    , framebufferRenderArea :: Vk.Extent2D 
    } deriving stock (Eq, Show)

-- | Collection of attachments represented by a Framebuffer 
data Attachments = Attachments
    { -- | Color attachment handle
      attachmentsHandle :: Vk.ImageView

      -- | ImageView extent. Should be the same on all attachments.
    , attachmentsExtent :: Vk.Extent2D 
    } deriving stock (Eq, Show)

-- | Bracket for creating/releasing a Vulkan Framebuffer
withFramebuffer
    :: MonadManaged m
    => Vk.Device
    -> Vk.RenderPass
    -> Attachments
    -> m Framebuffer
withFramebuffer device renderPass attachments = bracketManaged
    (createFramebuffer device renderPass attachments) (destroyFramebuffer device)

-- Creates a new render pass
createFramebuffer
    :: Vk.Device
    -> Vk.RenderPass
    -> Attachments
    -> IO Framebuffer
createFramebuffer device renderPass attachments = do
    handle <- Vk.createFramebuffer device (mkFramebufferCreateInfo renderPass attachments)
        Nothing

    pure (Framebuffer handle (attachmentsExtent attachments))

-- Destroys an existing framebuffer
destroyFramebuffer :: Vk.Device -> Framebuffer -> IO ()
destroyFramebuffer device framebuffer = Vk.destroyFramebuffer device
    (framebufferHandle framebuffer) Nothing

mkFramebufferCreateInfo
    :: Vk.RenderPass
    -> Attachments
    -> Vk.FramebufferCreateInfo '[]
mkFramebufferCreateInfo renderPass attachments = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.renderPass = renderPass
    , Vk.attachments = [attachmentsHandle attachments]
    , Vk.width = w
    , Vk.height = h
    , Vk.layers = 1
    }
  where
    Vk.Extent2D w h = attachmentsExtent attachments 