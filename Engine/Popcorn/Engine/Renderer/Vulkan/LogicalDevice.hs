{-# LANGUAGE DataKinds #-}

-- | Vulkan logical device
module Popcorn.Engine.Renderer.Vulkan.LogicalDevice
    ( -- * Resource creation/release
      withLogicalDevice
    ) where

import Control.Monad.Managed (MonadManaged)
import Data.Maybe (isJust)
import Data.Word (Word32)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (VulkanDevice(..))
import Popcorn.Engine.Renderer.Vulkan.Utils (formatList)

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk
import qualified Vulkan.Zero as Vk

-- | Bracket for creating/releasing a Vulkan logical device and its associated queues
withLogicalDevice
    :: MonadManaged m
    => VulkanDevice -- ^ The Vulkan Device
    -> Word32       -- ^ Index of the Queue family with Graphics support
    -> Maybe Word32 -- ^ Index of the Queue family with Presentation support 
    -> m Vk.Device
withLogicalDevice gd graphicsQueueFamilyIndex presentQueueFamilyIndex =
    bracketManaged
        (createLogicalDevice gd graphicsQueueFamilyIndex presentQueueFamilyIndex)
        destroyLogicalDevice

createLogicalDevice
    :: VulkanDevice
    -> Word32
    -> Maybe Word32
    -> IO Vk.Device
createLogicalDevice VulkanDevice{..} graphicsQueueFamilyIndex presentQueueFamilyIndex = do
    let deviceExts = V.fromList
            [Vk.KHR_SWAPCHAIN_EXTENSION_NAME | isJust presentQueueFamilyIndex]

    Vk.createDevice
        vdPhysicalDevice
        (mkDeviceCreateInfos graphicsQueueFamilyIndex presentQueueFamilyIndex deviceExts)
        Nothing
        <* engineLog (formatList "Vulkan device extensions" deviceExts)

destroyLogicalDevice :: Vk.Device -> IO ()
destroyLogicalDevice device = Vk.deviceWaitIdle device
    >> Vk.destroyDevice device Nothing

mkDeviceCreateInfos
    :: Word32
    -> Maybe Word32
    -> V.Vector BS.ByteString
    -> Vk.DeviceCreateInfo '[]
mkDeviceCreateInfos graphicsQueueFamilyIndex presentQueueFamilyIndex deviceExts =
    let indices = unique [Just graphicsQueueFamilyIndex, presentQueueFamilyIndex]

        queueCreateInfos = (>>= Just . (\i -> Vk.zero
                { Vk.queueFamilyIndex = i
                , Vk.queuePriorities = [1.0]
                })) <$> indices

        physicalDeviceFeatures = Vk.zero
    in
        Vk.zero
            { Vk.queueCreateInfos = Vk.SomeStruct <$> V.catMaybes queueCreateInfos
            , Vk.enabledExtensionNames = deviceExts
            , Vk.enabledFeatures = Just physicalDeviceFeatures
            }

-- Note: ordering may not be preserved
unique :: Ord a => [a] -> V.Vector a
unique = V.fromList . Set.toList . Set.fromList