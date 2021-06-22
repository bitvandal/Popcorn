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
import Control.Monad.Managed (Managed)
import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Data.Time (diffDays, fromGregorian, getCurrentTime, utctDay)
import Paths_Engine (version)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Platform.GLFW.Window (Window, eventsLoop)
import Popcorn.Engine.Settings (Settings)

import qualified Data.Text as T

import qualified Popcorn.Engine.Renderer.VulkanRenderer as R

-- | The Engine
data Engine = Engine
    { engineWindow :: Maybe Window
    , engineRenderer :: R.Renderer
    }

-- | Getters
getRenderer :: Engine -> R.Renderer
getRenderer Engine{..} = engineRenderer

-- | Set up The Engine for interactive mode
withEngineInteractive :: Application -> Window -> Settings -> Managed Engine
withEngineInteractive app window settings = do
    liftIO printEngineVersion

    let engineWindow = pure window

    R.withVulkanRenderer app window settings >>= \case
        Left err -> liftIO $ throwIO (EngineException err)
        Right renderer -> do
            let engineRenderer = R.RendererInteractive renderer
            pure Engine{..}

-- | Engine main entry point
run :: Engine -> IO ()
run engine = case engineRenderer engine of
    R.RendererInteractive r -> runInteractive engine r
    R.RendererStatic      _ -> pure ()

-- Engine run loop main entry point for interactive mode
runInteractive :: Engine -> R.VulkanRendererInteractive -> IO ()
runInteractive engine renderer = do
    let window = fromJust (engineWindow engine)
    eventsLoop window (R.frame4 renderer)

-- | Set up The Engine for rendering to a static image
withEngineRenderStatic :: Application -> Managed Engine
withEngineRenderStatic app = do
    liftIO printEngineVersion

    let engineWindow = Nothing

    R.withVulkanRendererStatic app >>= \case
        Left err -> liftIO $ throwIO (EngineException err)
        Right renderer -> do
            let engineRenderer = R.RendererStatic renderer
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