-- | GLFW platform
module Popcorn.Engine.Platform.GLFW.Init
    ( -- * Types
      -- Platform

      -- * Initialization
      withGlfwPlatform
    , withGlfwForVulkanPlatform
    ) where

import Control.Exception (bracket_, throwIO)
import Control.Monad (unless)
import Control.Monad.Managed (Managed, managed_)
import Data.Maybe (fromMaybe)

import Popcorn.Common.Log.Logger (platformLog)
import Popcorn.Engine.Platform.GLFW.Vulkan (supportsVulkan)
import Popcorn.Engine.Exception (EngineException(EngineException))

import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW

-- | The GLFW Platform
--data Platform = Platform

-- | Returns the GLFW platform with OpenGL support
withGlfwPlatform :: Managed ()
withGlfwPlatform = managed_ (bracket_ initForOpenGL terminate)

-- | Returns the GLFW platform ensuring Vulkan support, or throws an EngineException
withGlfwForVulkanPlatform :: Managed ()
withGlfwForVulkanPlatform = managed_ (bracket_ initForVulkan terminate)

initForOpenGL :: IO ()
initForOpenGL = initCommon

initForVulkan :: IO ()
initForVulkan = do
    initCommon

    platformSupportsVulkan <- supportsVulkan
    unless platformSupportsVulkan (throwIO (EngineException "[GLFW] Vulkan not detected"))

initCommon :: IO ()
initCommon = do
    result <- GLFW.init
    unless result (throwIO (EngineException "Failed to initialize GLFW platform"))

    glfwVersion <- GLFW.getVersionString

    platformLog (mconcat
        [ "Initialized GLFW platform version "
        , T.pack (fromMaybe "Unknown" glfwVersion)
        ])

terminate :: IO ()
terminate = GLFW.terminate >> platformLog "GLFW Platform has been terminated!"