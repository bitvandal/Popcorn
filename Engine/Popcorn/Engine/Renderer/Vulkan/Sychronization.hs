{-# LANGUAGE DataKinds #-}

-- | Vulkan synchronization primitives
module Popcorn.Engine.Renderer.Vulkan.Sychronization
    ( -- * Resource creation/destruction
      withSemaphore
    
      -- * Concurrency primitives
    , waitDeviceIdle
    ) where

import Control.Monad.Managed (MonadManaged)

import Popcorn.Engine.Managed.Extra (bracketManaged)

import qualified Vulkan.Core10 as Vk
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