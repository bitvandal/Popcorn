-- | GLFW platform Window supporting code
module Popcorn.Engine.Platform.GLFW.Window
    ( -- * Types
      Window(..)

      -- * Initialization
    , withWindow

      -- * Platform events loop
    , eventsLoop
    ) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forever, when)
import Control.Monad.Managed (Managed, managed)
import System.Exit (exitSuccess)

import Popcorn.Common.Log.Logger (platformLog)
import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Platform.GLFW.Utils
    ( glfwLastErrorFriendlyDesc
    , glfwLastErrorFriendlyDescIfAny
    )

import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW

-- | The GLFW Platform window
newtype Window = Window
    { windowHandle :: GLFW.Window
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
            return (Window handle)

        Nothing -> do
            err <- glfwLastErrorFriendlyDesc
            throwIO (EngineException ("[Platform] Failed to create Window. " <> err))

destroyWindow :: Window -> IO ()
destroyWindow Window{..} = GLFW.destroyWindow windowHandle

-- | Platform run loop
eventsLoop :: Window -> IO () -> IO ()
eventsLoop window frame = do
    forever $ do
        frame

        GLFW.pollEvents

        glfwLastErrorFriendlyDescIfAny >>= \case
            Left err -> platformLog ("Error while polling events. " <> err)
            Right _  -> pure ()

        shouldClose <- GLFW.windowShouldClose (windowHandle window)
        when shouldClose exitSuccess