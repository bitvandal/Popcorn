-- | GLFW platform Window supporting code
module Popcorn.Engine.Platform.GLFW.Window
    ( -- * Types
      WindowStatus(..)

      -- * Initialization
    , withWindow

      -- * Platform events loop
    , eventsLoop
    ) where

import Control.Exception (bracket, throwIO)
import Control.Monad.Managed (Managed, managed)

import Popcorn.Common.Log.Logger (platformLog)
import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Platform.GLFW.Internal.Window (Window(..))
import Popcorn.Engine.Platform.GLFW.Utils
    ( glfwLastErrorFriendlyDesc
    , glfwLastErrorFriendlyDescIfAny
    )
import Popcorn.Engine.Renderer.Renderer (FrameStatus(..))
import Popcorn.Engine.Settings (Settings(..), WindowMode(..))

import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW

-- | Returns the GLFW Platform Window
withWindow :: Application -> Settings -> Managed Window
withWindow app settings = managed (bracket (createWindow app settings) destroyWindow)

createWindow :: Application -> Settings -> IO Window
createWindow Application{..} settings = do
    GLFW.windowHint (GLFW.WindowHint'CenterCursor False)
    GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
    GLFW.windowHint (GLFW.WindowHint'Resizable (sWindowMode settings == WindowedResizable))

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

-- | Platform window status
data WindowStatus
    = WindowStatusShouldClose   -- ^ The user requested closing the window
    | WindowStatusStateChanged  -- ^ The Window state has changed and requires some action
    deriving stock (Eq, Show)

-- | Platform run loop. Ends when there is a change to the platform window state
eventsLoop
    :: Window           -- ^ Platform Window
    -> IO FrameStatus   -- ^ Rendering Callback
    -> IO WindowStatus  -- ^ Returns the current WindowStatus that caused the loop to end
eventsLoop window frame = do
    frame >>= \case
        FrameStatusOK -> do
            GLFW.pollEvents

            glfwLastErrorFriendlyDescIfAny >>= \case
                Left err -> platformLog ("Error while polling events. " <> err)
                Right _  -> pure ()

            GLFW.windowShouldClose (windowHandle window) >>= \case
                True  -> pure WindowStatusShouldClose
                False -> eventsLoop window frame

        FrameStatusRenderContextInvalid -> pure WindowStatusStateChanged