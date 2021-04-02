-- | The Engine.
module Popcorn.Engine.Engine
    ( -- * Types
      Engine

      -- * Initialization
    , withEngine

      -- * Engine Information
    , showEngineVersion
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed)
import Data.Version (showVersion)
import Data.Time (diffDays, fromGregorian, getCurrentTime, utctDay)
import Paths_Engine (version)

import Popcorn.Common.Log.Logger (engineLog)

import qualified Data.Text as T

-- | The Engine
data Engine = Engine

-- | Returns The Engine
withEngine :: Managed Engine
withEngine = do
    engineVersion <- liftIO showEngineVersion
    liftIO (engineLog $ mconcat [
        "Engine version: "
        <> engineVersion
        ])
    return Engine

-- | Returns a formatted Engine version number
showEngineVersion :: IO T.Text
showEngineVersion = do
    buildNumber <- showEngineBuildNumber
    return $ mconcat [
        T.pack (showVersion version)
        <> " (build number "
        <> buildNumber
        <> ")"
        ]

-- | Returns a formatted Engine build number (number of days since project was started)
showEngineBuildNumber :: IO T.Text
showEngineBuildNumber = do
    now <- utctDay <$> getCurrentTime

    let genesis = fromGregorian 2021 01 31
        buildNumber = diffDays now genesis

    return (T.pack (show buildNumber))