-- | Vulkan surfaces (general code)
module Popcorn.Engine.Renderer.Vulkan.Surface
    ( -- * Surface creation/destruction
      withVulkanSurface

      -- * Surface queries
    , findPresentQueueFamilyIndex
    ) where

import Control.Applicative (Applicative(liftA2))
import Control.Monad.Managed (MonadManaged)
import Data.Word (Word32)

import Popcorn.Common.Control.Monad.Extra (findM)
import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Platform.GLFW.Window (Window(..))
import Popcorn.Engine.Renderer.Vulkan.Internal.Surface (Surface(..), querySurfaceInfo)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (VulkanDevice(..))
import Popcorn.Engine.Renderer.Vulkan.Platform.GLFW
    ( createVulkanSurface
    , supportsPresentationPlatform
    )

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk

-- | Manages a Vulkan WSI surface for a specific platform window
withVulkanSurface
    :: MonadManaged m
    => Window
    -> Vk.Instance
    -> Vk.PhysicalDevice
    -> m Surface
withVulkanSurface window inst physicalDevice = bracketManaged
    (createSurface window inst physicalDevice)
    (destroySurface inst)

-- Create a Vulkan WSI surface and gets its properties 
createSurface
    :: Window
    -> Vk.Instance
    -> Vk.PhysicalDevice
    -> IO Surface
createSurface window inst physicalDevice = do
    surface <- createVulkanSurface window inst
    surfaceInfo <- querySurfaceInfo physicalDevice surface
    pure (Surface
        { surfaceHandle = surface
        , surfaceInfo = surfaceInfo
        })

-- Destroy a Vulkan WSI Surface
destroySurface :: Vk.Instance -> Surface -> IO ()
destroySurface inst surface =
    Vk.destroySurfaceKHR inst (surfaceHandle surface) Nothing

-- | Returns the first queue family with queues that support presentation
findPresentQueueFamilyIndex
    :: Vk.Instance
    -> VulkanDevice
    -> Surface
    -> IO (Either T.Text Word32)
findPresentQueueFamilyIndex inst VulkanDevice{..} surface = do
    let props = V.zip [0..] vdQueueFamilyProperties

    found <- fmap fst <$> findM (canPresent inst vdPhysicalDevice surface) props

    pure $ maybe (Left "[Renderer] Could not obtain a present queue family") Right found

canPresent
    :: Vk.Instance
    -> Vk.PhysicalDevice
    -> Surface
    -> (Word32, Vk.QueueFamilyProperties)
    -> IO Bool
canPresent inst physicalDevice surface (queueFamilyIndex, props) =
    liftA2 (&&)
        (supportsPresentationPlatform inst physicalDevice (queueFamilyIndex, props))
        (supportsPresentationSurface physicalDevice queueFamilyIndex surface)

-- Check if a queue family of a physical device supports presentation to a given surface
supportsPresentationSurface
    :: Vk.PhysicalDevice
    -> Word32
    -> Surface
    -> IO Bool
supportsPresentationSurface physicalDevice queueFamilyIndex Surface{..} =
    Vk.getPhysicalDeviceSurfaceSupportKHR physicalDevice
        queueFamilyIndex surfaceHandle