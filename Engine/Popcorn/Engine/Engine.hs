-- | The Engine.
module Popcorn.Engine.Engine
    ( -- * Types
      Engine

      -- * Initialization
    , withEngineInteractive
    , withEngineRenderOffscreen

      -- * Engine Information
    , showEngineVersion
    ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed)
import Data.Version (showVersion)
import Data.Time (diffDays, fromGregorian, getCurrentTime, utctDay)
import Paths_Engine (version)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Platform.GLFW.Window (Window)
import Popcorn.Engine.Settings (Settings)

import qualified Data.Text as T

import qualified Popcorn.Engine.Renderer.Vulkan as R

-- | The Engine
data Engine = Engine
    { engineWindow :: Maybe Window
    , engineRenderer :: R.Renderer
    }

-- | Set up The Engine for interactive mode
withEngineInteractive :: Application -> Window -> Settings -> Managed Engine
withEngineInteractive app window settings = do
    liftIO printEngineVersion

    let engineWindow = pure window

    R.withVulkanRenderer app window settings >>= \case
        Left err -> liftIO $ throwIO (EngineException err)
        Right engineRenderer -> pure Engine{..}

-- | Set up The Engine for off-screen rendering mode
withEngineRenderOffscreen :: Application -> Managed Engine
withEngineRenderOffscreen app = do
    liftIO printEngineVersion

    let engineWindow = Nothing

    R.withVulkanRendererOffscreen app >>= \case
        Left err -> liftIO $ throwIO (EngineException err)
        Right engineRenderer -> pure Engine{..}

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