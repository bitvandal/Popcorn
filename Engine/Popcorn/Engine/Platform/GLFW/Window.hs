-- | GLFW platform Window supporting code
module Popcorn.Engine.Platform.GLFW.Window
    ( -- * Types
      Window(..)
      -- * Initialization
    , withWindow
    ) where

import Control.Exception (bracket, throwIO)
import Control.Monad.Managed (Managed, managed)

import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Platform.GLFW.Utils (glfwLastErrorFriendlyDesc)

import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Extensions.VK_KHR_surface as Vk

-- | The GLFW Platform window
data Window = Window
    { windowHandle :: GLFW.Window
    , windowVulkanSurface :: Maybe Vk.SurfaceKHR
    }

-- | Returns the GLFW Platform Window
withWindow :: Application -> Managed Window
withWindow app = managed (bracket (createWindow app) destroyWindow)

createWindow :: Application -> IO Window
createWindow Application{..} = do
    GLFW.windowHint (GLFW.WindowHint'CenterCursor False)
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)

    let width = applicationMainWindowWidth
    let height = applicationMainWindowHeight
    let title = "Popcorn::" <> applicationName

    GLFW.createWindow width height (T.unpack title) Nothing Nothing >>= \case
        Just handle ->
            return (Window handle Nothing)

        Nothing -> do
            err <- glfwLastErrorFriendlyDesc
            throwIO (EngineException ("[Platform] Failed to create Window. " <> err))

destroyWindow :: Window -> IO ()
destroyWindow Window{..} = GLFW.destroyWindow windowHandle