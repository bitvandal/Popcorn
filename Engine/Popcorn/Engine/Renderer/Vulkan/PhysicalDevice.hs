{-# LANGUAGE DuplicateRecordFields #-}

-- | Vulkan Physical Devices
module Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( GraphicsDevice(..)
    , selectGraphicsDevice
    , graphicsDeviceFriendlyDesc
    ) where

import Data.Word (Word32)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Version as Vk

-- | The Vulkan Physical Device
data GraphicsDevice = GraphicsDevice
    { gdPhysicalDevice :: Vk.PhysicalDevice
    , gdProperties :: Vk.PhysicalDeviceProperties
    , gdQueueFamilyProperties :: V.Vector Vk.QueueFamilyProperties
    , gdFeatures :: Vk.PhysicalDeviceFeatures
    , gdMemoryProperties :: Vk.PhysicalDeviceMemoryProperties
    }

-- TODO at least ensure that what is returned is a graphical device
-- TODO what if no devices are returned?
-- TODO exception handling on all calls

-- | Enumerates the Vulkan Physical Devices and returns the first graphics device found
selectGraphicsDevice :: Vk.Instance -> IO GraphicsDevice
selectGraphicsDevice vkInstance =
    Vk.enumeratePhysicalDevices vkInstance >>= \case
        (Vk.SUCCESS, physicalDevices) -> getPhysicalDeviceInfo (V.head physicalDevices)
        (_, _)                        -> undefined -- TODO return (Left (T.pack (show result)))

getPhysicalDeviceInfo :: Vk.PhysicalDevice -> IO GraphicsDevice
getPhysicalDeviceInfo vkPhysicalDevice = GraphicsDevice vkPhysicalDevice
    <$> Vk.getPhysicalDeviceProperties vkPhysicalDevice
    <*> Vk.getPhysicalDeviceQueueFamilyProperties vkPhysicalDevice
    <*> Vk.getPhysicalDeviceFeatures vkPhysicalDevice
    <*> Vk.getPhysicalDeviceMemoryProperties vkPhysicalDevice

-- | Returns a graphics device textual description
graphicsDeviceFriendlyDesc :: GraphicsDevice -> T.Text
graphicsDeviceFriendlyDesc GraphicsDevice{..} = mconcat
    [ deviceTypeDesc (Vk.deviceType gdProperties)
    , " GPU: "
    , T.decodeUtf8 (Vk.deviceName gdProperties)
    , " (Vulkan driver version "
    , apiVersionDesc (Vk.apiVersion (gdProperties :: Vk.PhysicalDeviceProperties))
    , ") "
    ]

apiVersionDesc :: Word32 -> T.Text
apiVersionDesc apiVersion = mconcat
    [ T.pack (show (Vk._VERSION_MAJOR apiVersion))
    , "."
    , T.pack (show (Vk._VERSION_MINOR apiVersion))
    , "."
    , T.pack (show (Vk._VERSION_PATCH apiVersion))
    ]

deviceTypeDesc :: Vk.PhysicalDeviceType -> T.Text
deviceTypeDesc = \case
    Vk.PHYSICAL_DEVICE_TYPE_OTHER -> "Other"
    Vk.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> "Integrated"
    Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> "Discrete"
    Vk.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> "Virtual"
    Vk.PHYSICAL_DEVICE_TYPE_CPU -> "CPU"