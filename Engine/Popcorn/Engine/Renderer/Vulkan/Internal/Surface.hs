module Popcorn.Engine.Renderer.Vulkan.Internal.Surface
    ( -- * Types
      Surface(..)
    , SurfaceInfo(..)

      -- * Surface management
    , querySurfaceInfo
    ) where

import Control.Monad (when)

import Popcorn.Common.Log.Logger (engineLog)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk

-- | WSI Vulkan surface
newtype Surface = Surface
    { surfaceHandle :: Vk.SurfaceKHR
    }

-- | Vulkan surface capabilities, supported formats and present modes
data SurfaceInfo = SurfaceInfo
    { surfaceInfoCapabilities :: Vk.SurfaceCapabilitiesKHR
    , surfaceInfoFormats :: V.Vector Vk.SurfaceFormatKHR
    , surfaceInfoPresentModes :: V.Vector Vk.PresentModeKHR
    } deriving stock Show

-- | Query Vulkan surface properties
querySurfaceInfo :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO SurfaceInfo
querySurfaceInfo physicalDevice surface = SurfaceInfo
    <$> Vk.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
    <*> querySurfaceFormats physicalDevice surface
    <*> queryPresentModes physicalDevice surface

-- Queries color formats supported by surface
querySurfaceFormats
    :: Vk.PhysicalDevice
    -> Vk.SurfaceKHR
    -> IO (V.Vector Vk.SurfaceFormatKHR)
querySurfaceFormats physicalDevice surface = do
    (result, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface

    when (result /= Vk.SUCCESS) $ engineLog
        ("Warning: while checking for surface formats =" <> T.pack (show result))

    pure formats

-- Queries supported presentation modes
queryPresentModes
    :: Vk.PhysicalDevice
    -> Vk.SurfaceKHR
    -> IO (V.Vector Vk.PresentModeKHR)
queryPresentModes physicalDevice surface = do
    (result, modes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface

    when (result /= Vk.SUCCESS) $ engineLog
        ("Warning: while checking for present modes =" <> T.pack (show result))

    pure modes