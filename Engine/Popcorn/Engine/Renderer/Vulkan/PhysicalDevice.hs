-- | Vulkan Physical Devices
module Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( -- * Data types
      VulkanDevice(..)
    , QueueFamily(..)

      -- * Device selection
    , selectInteractiveGraphicsDevice
    , selectGraphicsDevice

      -- * Device Queue functions
    , findGraphicsQueueFamilyIndex

      -- * Utilities
    , vulkanDeviceFriendlyDesc
    ) where

import Control.Monad (when)
import Data.Bits (Bits((.&.)), zeroBits)
import Data.Word (Word32)

import Popcorn.Common.Control.Monad.Extra (findM)
import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Common.Utils (maybeToEither)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk
import qualified Vulkan.Version as Vk

-- | The Vulkan Physical Device (a Vulkan Device may or may not have graphics support)
data VulkanDevice = VulkanDevice
    { vdPhysicalDevice :: Vk.PhysicalDevice
    , vdProperties :: Vk.PhysicalDeviceProperties
    , vdQueueFamilyProperties :: V.Vector Vk.QueueFamilyProperties
    , vdFeatures :: Vk.PhysicalDeviceFeatures
    , vdMemoryProperties :: Vk.PhysicalDeviceMemoryProperties
    }

-- | Vulkan Queue Family
data QueueFamily = QueueFamily
    { queueFamilyIndex :: Word32        -- ^ Index of the queue family
    , queueFamilyHandle :: Vk.Queue     -- ^ Queue handle
    }

-- | Enumerates Vulkan Physical Devices and returns the first graphics device found
-- with Swapchain support
selectInteractiveGraphicsDevice :: Vk.Instance -> IO (Either T.Text VulkanDevice)
selectInteractiveGraphicsDevice =
    findDeviceBy (\device -> do
        let isGraphics = isGraphicsDevice device
        hasSwapchain <- hasSwapchainSupport device
        pure (isGraphics && hasSwapchain))

-- | Enumerates Vulkan Physical Devices and returns the first graphics device found
-- suitable for rendering static images
selectGraphicsDevice :: Vk.Instance -> IO (Either T.Text VulkanDevice)
selectGraphicsDevice = findDeviceBy (pure . isGraphicsDevice)

-- | Returns the first Graphics Queue Family of a given VulkanDevice
findGraphicsQueueFamilyIndex :: VulkanDevice -> Either T.Text Word32
findGraphicsQueueFamilyIndex VulkanDevice{..} = maybe
    (Left "[Renderer] Could not retrieve a graphics queue family index") Right found
  where
    properties = V.zip [0..] vdQueueFamilyProperties
    found = fst <$> V.find (\(_, p) -> hasGraphicsQueue p) properties

-- Finds the first physical device matching a filter
findDeviceBy
    :: (VulkanDevice -> IO Bool)
    -> Vk.Instance
    -> IO (Either T.Text VulkanDevice)
findDeviceBy condition vkInstance = do
    Vk.enumeratePhysicalDevices vkInstance >>= \case
        (Vk.SUCCESS, physicalDevices)    -> findDevice physicalDevices
        (Vk.INCOMPLETE, physicalDevices) -> findDevice physicalDevices
        _ -> pure (Left "[Renderer] Could not automatically select a Graphics device!")
  where
    findDevice physicalDevices = do
        devicesInfo <- traverse getPhysicalDeviceInfo physicalDevices
        maybeToEither "[Renderer] No suitable Graphics device was found in the system!"
            <$> findM condition devicesInfo

-- True if the device has any queue family with queues supporting graphics operations
isGraphicsDevice :: VulkanDevice -> Bool
isGraphicsDevice VulkanDevice{..} = any hasGraphicsQueue vdQueueFamilyProperties

-- True if the device supports the VK_KHR_swapchain extension
hasSwapchainSupport :: VulkanDevice -> IO Bool
hasSwapchainSupport VulkanDevice{..} = do
    (vkResult, extsProperties) <-
        Vk.enumerateDeviceExtensionProperties vdPhysicalDevice Nothing

    when (vkResult /= Vk.SUCCESS) $ engineLog
        ("Warning: while checking for swapchain support=" <> T.pack (show vkResult))

    pure (V.any ((Vk.KHR_SWAPCHAIN_EXTENSION_NAME ==) . Vk.extensionName) extsProperties)

hasGraphicsQueue :: Vk.QueueFamilyProperties -> Bool
hasGraphicsQueue queueFamilyProps = 
    Vk.queueFlags queueFamilyProps .&. Vk.QUEUE_GRAPHICS_BIT /= zeroBits 

getPhysicalDeviceInfo :: Vk.PhysicalDevice -> IO VulkanDevice
getPhysicalDeviceInfo vkPhysicalDevice = VulkanDevice vkPhysicalDevice
    <$> Vk.getPhysicalDeviceProperties vkPhysicalDevice
    <*> Vk.getPhysicalDeviceQueueFamilyProperties vkPhysicalDevice
    <*> Vk.getPhysicalDeviceFeatures vkPhysicalDevice
    <*> Vk.getPhysicalDeviceMemoryProperties vkPhysicalDevice

-- | Returns a graphics device textual description
vulkanDeviceFriendlyDesc :: VulkanDevice -> T.Text
vulkanDeviceFriendlyDesc VulkanDevice{..} = mconcat
    [ deviceTypeDesc (Vk.deviceType vdProperties)
    , " GPU: "
    , T.decodeUtf8 (Vk.deviceName vdProperties)
    , " (Vulkan driver version "
    , apiVersionDesc (Vk.apiVersion (vdProperties :: Vk.PhysicalDeviceProperties))
    , ") "
    ]

apiVersionDesc :: Word32 -> T.Text
apiVersionDesc apiVersion = mconcat
    [ T.pack (show (Vk._API_VERSION_MAJOR apiVersion))
    , "."
    , T.pack (show (Vk._API_VERSION_MINOR apiVersion))
    , "."
    , T.pack (show (Vk._API_VERSION_PATCH apiVersion))
    ]

deviceTypeDesc :: Vk.PhysicalDeviceType -> T.Text
deviceTypeDesc = \case
    Vk.PHYSICAL_DEVICE_TYPE_OTHER -> "Other"
    Vk.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> "Integrated"
    Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> "Discrete"
    Vk.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> "Virtual"
    Vk.PHYSICAL_DEVICE_TYPE_CPU -> "CPU"