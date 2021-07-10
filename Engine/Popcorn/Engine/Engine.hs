-- | The Engine. Use it qualified as Engine.
module Popcorn.Engine.Engine
    ( -- * Types
      Engine

      -- * Initialization
    , withEngineInteractive
    , withEngineRenderStatic

      -- * Run loop
    , run

      -- * Engine Information
    , showEngineVersion

      -- * Accessors
    , getRenderer
    ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed, MonadManaged, with)
import Data.Maybe (fromJust)
import Data.Time (diffDays, fromGregorian, getCurrentTime, utctDay)
import Data.Version (showVersion)
import Paths_Engine (version)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Platform.GLFW.Internal.Window (Window)
import Popcorn.Engine.Platform.GLFW.Window (WindowStatus(..), eventsLoop)
import Popcorn.Engine.Renderer.Renderer (FrameStatus (FrameStatusOK), frameRender)
import Popcorn.Engine.Renderer.Vulkan.Internal.Vulkan (VulkanRenderContext)
import Popcorn.Engine.Settings (Settings(..))

import qualified Data.Text as T

import qualified Popcorn.Engine.Renderer.VulkanRenderer as R

-- | The Engine
data Engine = Engine
    { engineWindow :: Maybe Window
    , engineRenderer :: R.Renderer
    , engineSettings :: Settings
    }

-- | Getters
getRenderer :: Engine -> R.Renderer
getRenderer Engine{..} = engineRenderer

-- | Set up The Engine for interactive mode
withEngineInteractive :: Application -> Window -> Settings -> Managed Engine
withEngineInteractive app window settings = do
    liftIO printEngineVersion

    let engineWindow = pure window

    R.withVulkanRenderer app window >>= \case
        Left err -> liftIO $ throwIO (EngineException err)
        Right renderer -> do
            let engineRenderer = R.RendererInteractive renderer
                engineSettings = settings
            pure Engine{..}

-- | Engine main entry point
run :: MonadManaged m => Engine -> m ()
run engine = case engineRenderer engine of
    R.RendererStatic      _ -> pure ()
    R.RendererInteractive r -> liftIO $ runInteractive engine r (engineSettings engine)

-- Engine run loop main entry point for interactive mode
runInteractive :: Engine -> R.VulkanRendererInteractive -> Settings -> IO ()
runInteractive engine renderer settings = do
    let window = fromJust (engineWindow engine)
        renderContext = R.withVulkanRenderContext renderer settings

    result <- with renderContext (eventsLoop window . frame engine)

    case result of
        WindowStatusStateChanged -> runInteractive engine renderer settings
        WindowStatusShouldClose  -> pure ()

-- Engine Frame entry point. Invokes all engine modules.
--
-- Note: eventually, the idea is invoke here a `frame<engine_component>` function for each
-- component, and this function will be invoked from the run loop with the desired
-- periodicity.
frame :: Engine -> VulkanRenderContext -> IO FrameStatus
frame Engine{..} rc = case engineRenderer of
    R.RendererInteractive _ -> frameRender rc
    R.RendererStatic R.VulkanRendererStatic{} -> pure FrameStatusOK

-- | Set up The Engine for rendering to a static image
withEngineRenderStatic :: Application -> Settings -> Managed Engine
withEngineRenderStatic app settings = do
    liftIO printEngineVersion

    let engineWindow = Nothing

    R.withVulkanRendererStatic app >>= \case
        Left err -> liftIO $ throwIO (EngineException err)
        Right renderer -> do
            let engineRenderer = R.RendererStatic renderer
                engineSettings = settings
            pure Engine{..}

printEngineVersion :: IO ()
printEngineVersion = do
    engineVersion <- showEngineVersion
    liftIO (engineLog $ mconcat
        [ "Engine version: "
        , engineVersion
        ])

-- | Returns a formatted Engine version number
showEngineVersion :: IO T.Text
showEngineVersion = do
    buildNumber <- showEngineBuildNumber
    return $ mconcat
        [ T.pack (showVersion version)
        , " (build number "
        , buildNumber
        , ")"
        ]

-- | Returns a formatted Engine build number (number of days since project was started)
showEngineBuildNumber :: IO T.Text
showEngineBuildNumber = do
    now <- utctDay <$> getCurrentTime

    let genesis = fromGregorian 2021 01 31
        buildNumber = diffDays now genesis

    pure (T.pack (show buildNumber))