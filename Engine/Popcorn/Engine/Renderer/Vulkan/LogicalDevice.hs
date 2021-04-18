{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Vulkan logical device
module Popcorn.Engine.Renderer.Vulkan.LogicalDevice
    ( -- * Resource creation/release
      withLogicalDevice
    ) where

import Control.Monad.Managed (MonadManaged)

import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (GraphicsDevice(..))

import qualified Vulkan.Core10 as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk

-- | Bracket for creating/releasing a Vulkan logical device
withLogicalDevice
    :: MonadManaged m
    => GraphicsDevice
    -> m Vk.Device
withLogicalDevice gd = bracketManaged (createLogicalDevice gd) destroyLogicalDevice

createLogicalDevice :: GraphicsDevice -> IO Vk.Device
createLogicalDevice GraphicsDevice{..} =
    Vk.createDevice gdPhysicalDevice mkDeviceCreateInfo Nothing

destroyLogicalDevice :: Vk.Device -> IO ()
destroyLogicalDevice device = Vk.deviceWaitIdle device
    >> Vk.destroyDevice device Nothing

mkDeviceCreateInfo :: Vk.DeviceCreateInfo '[]
mkDeviceCreateInfo =
    let
        deviceQueueCreateInfo = Vk.zero
            { Vk.queueFamilyIndex = 0 -- fromIntegral qfi
            , Vk.queuePriorities = [1.0]
            }
        physicalDeviceFeatures = Vk.zero
    in
        Vk.zero
            { Vk.queueCreateInfos = [Vk.SomeStruct deviceQueueCreateInfo]
            , Vk.enabledExtensionNames = []
            , Vk.enabledFeatures = Just physicalDeviceFeatures
            }