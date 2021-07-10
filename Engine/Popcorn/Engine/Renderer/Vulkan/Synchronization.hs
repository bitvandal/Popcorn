{-# LANGUAGE DataKinds #-}

-- | Vulkan synchronization primitives
module Popcorn.Engine.Renderer.Vulkan.Synchronization
    ( -- * Resource creation/destruction
      withSemaphore
    
      -- * Concurrency primitives
    , waitDeviceIdle

      -- * Helper functions
    , unsignalSemaphoreSlow
    ) where

import Control.Monad.Managed (MonadManaged)

import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan (VulkanRendererInteractive(..))
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (QueueFamily(..))
  
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

-- | Bracket for creating/releasing a Vulkan semaphore
withSemaphore :: MonadManaged m => Vk.Device -> m Vk.Semaphore
withSemaphore device = bracketManaged (createSemaphore device) (destroySemaphore device)

createSemaphore :: Vk.Device -> IO Vk.Semaphore
createSemaphore device = Vk.createSemaphore device mkSemaphoreCreateInfo Nothing

mkSemaphoreCreateInfo :: Vk.SemaphoreCreateInfo '[]
mkSemaphoreCreateInfo = Vk.zero { Vk.flags = Vk.zero }

destroySemaphore :: Vk.Device -> Vk.Semaphore -> IO ()
destroySemaphore device semaphore = Vk.destroySemaphore device semaphore Nothing

-- | Waits until Vulkan device has finished all its work
waitDeviceIdle :: Vk.Device -> IO ()
waitDeviceIdle = Vk.deviceWaitIdle

-- | Unsignals a semaphore by making it wait on a empty command buffer submission to an
-- arbitrary queue.
--
-- Note: this is a slow operation as it waits for the device to be idle.
unsignalSemaphoreSlow :: VulkanRendererInteractive -> Vk.Semaphore -> IO ()
unsignalSemaphoreSlow VulkanRendererInteractive{..} semaphore = do
    let submitInfo = Vk.SomeStruct Vk.zero
            { Vk.waitSemaphores   = [semaphore]
            , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
            , Vk.commandBuffers   = []
            , Vk.signalSemaphores = []
            }
        queueHandle = queueFamilyHandle rGraphicsQueue

    Vk.queueSubmit queueHandle [submitInfo] Vk.NULL_HANDLE 

    Vk.deviceWaitIdle rLogicalDevice