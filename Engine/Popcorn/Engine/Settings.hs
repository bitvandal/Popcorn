-- | Engine settings
module Popcorn.Engine.Settings
    ( -- * Data types
      Settings(..)
    , VerticalSyncMode(..)
    , WindowMode(..)

    -- * Stock configurations
    , defaultSettings
    ) where

-- | Engine settings
data Settings = Settings
    { sVerticalSync :: VerticalSyncMode
    , sWindowMode :: WindowMode
    } deriving stock (Eq, Show)

-- | V-Sync modes
data VerticalSyncMode =
      NoVSync
    | WhatVSyncModesDoIWant
    deriving stock (Eq, Show)

-- | Platform Window mode
data WindowMode
    = Windowed
    | WindowedResizable
    deriving stock (Eq, Show)

-- | Stock default settings
defaultSettings:: Settings
defaultSettings = Settings
    { sVerticalSync = NoVSync
    , sWindowMode = Windowed
    }