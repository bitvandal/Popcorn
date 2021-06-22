{-# LANGUAGE  DataKinds #-}

-- | Vulkan Render Pass
module Popcorn.Engine.Renderer.Vulkan.RenderPass
    ( -- * Resource creation/release
      withRenderPass

      -- * Render pass commands
    , useRenderPass
    ) where

import Control.Monad.Managed (MonadManaged)
import Data.Maybe (maybeToList)

import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Zero as Vk

import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.Framebuffer (Framebuffer(..))

-- | Bracket for creating/releasing a Vulkan Render Pass
withRenderPass
    :: MonadManaged m
    => Vk.Device
    -> Vk.Format
    -> Bool
    -> m Vk.RenderPass
withRenderPass device colorAttachmentFormat clearScreen =
    bracketManaged
        (createRenderPass device colorAttachmentFormat clearScreen)
        (destroyRenderPass device)

-- Creates a new render pass
createRenderPass :: Vk.Device -> Vk.Format -> Bool -> IO Vk.RenderPass
createRenderPass device colorAttachmentFormat clearScreen = Vk.createRenderPass
    device (mkRenderPassCreateInfo colorAttachmentFormat clearScreen) Nothing

-- Destroys an existing render pass
destroyRenderPass :: Vk.Device -> Vk.RenderPass -> IO ()
destroyRenderPass device renderPass = Vk.destroyRenderPass device renderPass Nothing

mkRenderPassCreateInfo :: Vk.Format -> Bool -> Vk.RenderPassCreateInfo '[]
mkRenderPassCreateInfo colorAttachmentFormat clearScreen = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.attachments = [colorAttachmentDescription]
    , Vk.subpasses = [subpassDescription]
    , Vk.dependencies = [beginSubpassDependency, endSubpassDependency]
    }
  where
    colorAttachmentDescription =
        mkColorAttachmentDescription colorAttachmentFormat clearScreen
    colorAttachmentReference = Vk.zero
        { Vk.attachment = 0
        , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        }
    subpassDescription = Vk.zero
        { Vk.flags = Vk.zero
        , Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.inputAttachments = []
        , Vk.colorAttachments = [colorAttachmentReference]
        , Vk.resolveAttachments = []
        , Vk.depthStencilAttachment = Nothing
        , Vk.preserveAttachments = []
        }
    beginSubpassDependency = Vk.zero
        { Vk.srcSubpass = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass = 0
        , Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.srcAccessMask = Vk.zero
        , Vk.dstAccessMask = if clearScreen
            then Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            else Vk.ACCESS_COLOR_ATTACHMENT_READ_BIT 
        , Vk.dependencyFlags = Vk.zero
        }
    endSubpassDependency = Vk.zero
        { Vk.srcSubpass = 0
        , Vk.dstSubpass = Vk.SUBPASS_EXTERNAL
        , Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.dstStageMask = Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
        , Vk.srcAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        , Vk.dstAccessMask = Vk.zero
        , Vk.dependencyFlags = Vk.zero
        }

-- Make an VkAttachmentDescription for a color attachment. clearScreen boolean flag
-- indicates whether the attachment first will be cleared.
mkColorAttachmentDescription :: Vk.Format -> Bool -> Vk.AttachmentDescription
mkColorAttachmentDescription format clearScreen = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.format = format
    , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
    , Vk.loadOp = if clearScreen 
        then Vk.ATTACHMENT_LOAD_OP_CLEAR
        else Vk.ATTACHMENT_LOAD_OP_DONT_CARE
    , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
    , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
    , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
    , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
    , Vk.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
    }

-- | Wrap an IO action between vkCmdBeginRenderPass and vkCmdEndRenderPass calls
useRenderPass
    :: Vk.RenderPass
    -> Vk.CommandBuffer
    -> Framebuffer
    -> Maybe Vk.ClearValue
    -> IO a
    -> IO a
useRenderPass renderPass commandBuffer framebuffer clearValue action =
    Vk.cmdBeginRenderPass commandBuffer
        (mkRenderPassBeginInfo renderPass framebuffer clearValue)
        Vk.SUBPASS_CONTENTS_INLINE *> action <* Vk.cmdEndRenderPass commandBuffer

mkRenderPassBeginInfo
    :: Vk.RenderPass
    -> Framebuffer
    -> Maybe Vk.ClearValue
    -> Vk.RenderPassBeginInfo '[]
mkRenderPassBeginInfo renderPass framebuffer clearValue = Vk.zero
    { Vk.renderPass = renderPass
    , Vk.framebuffer = framebufferHandle framebuffer
    , Vk.renderArea = Vk.Rect2D (Vk.Offset2D 0 0) (framebufferRenderArea framebuffer)
    , Vk.clearValues = V.fromList (maybeToList clearValue)
    }