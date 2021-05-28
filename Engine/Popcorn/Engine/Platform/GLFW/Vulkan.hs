-- | GLFW platform Vulkan supporting code
module Popcorn.Engine.Platform.GLFW.Vulkan
    ( -- * Vulkan Setup
      supportsVulkan
    ) where

import Popcorn.Common.Log.Logger (platformLog)
import Popcorn.Engine.Platform.GLFW.Utils (glfwLastErrorFriendlyDesc)

import qualified Graphics.UI.GLFW as GLFW

-- | True if the GLFW has a minimal Vulkan support
supportsVulkan :: IO Bool
supportsVulkan =
    GLFW.vulkanSupported >>= \case
        True -> return True
        False -> do
            err <- glfwLastErrorFriendlyDesc
            platformLog ("Failed while checking Vulkan support. " <> err)
            return False