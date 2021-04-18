{-# LANGUAGE DuplicateRecordFields #-}

-- | Vulkan Physical Devices
module Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( GraphicsDevice(..)
    , selectGraphicsDevice
    , graphicsDeviceFriendlyDesc
    ) where

import Control.Exception (throwIO)
import Data.Word (Word32)

import Popcorn.Engine.Exception (EngineException(EngineException))

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

-- | Enumerates Vulkan Physical Devices and returns the first Vulkan capable device found
selectGraphicsDevice :: Vk.Instance -> IO GraphicsDevice
selectGraphicsDevice vkInstance =
    Vk.enumeratePhysicalDevices vkInstance >>= \case
        (Vk.SUCCESS, physicalDevices)    -> getFirstPhysicalDeviceInfo physicalDevices
        (Vk.INCOMPLETE, physicalDevices) -> getFirstPhysicalDeviceInfo physicalDevices
        (_, _) ->
            throwIO (EngineException "Could not automatically select a Graphics device!")

getFirstPhysicalDeviceInfo :: V.Vector Vk.PhysicalDevice -> IO GraphicsDevice
getFirstPhysicalDeviceInfo physicalDevices =
    if V.null physicalDevices
        then throwIO (EngineException "There are no compatible Vulkan Graphics devices!")
        else getPhysicalDeviceInfo (V.head physicalDevices)

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