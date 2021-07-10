-- | Engine settings
module Popcorn.Engine.Settings
    ( -- * Data types
      Settings(..)
    , VerticalSyncMode(..)
    , WindowMode(..)

      -- * Stock configurations
    , defaultSettings
    ) where

-- | Engine settings. Dynamic, can be updated at runtime.
data Settings = Settings
    { sVerticalSync :: VerticalSyncMode
    , sWindowMode :: WindowMode
    } deriving stock (Eq)

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

instance Show Settings where
    show settings = mconcat
        [ "Settings {VerticalSync = "
        , show (sVerticalSync settings)
        , ", WindowMode = "
        , show (sWindowMode settings)
        , "}"
        ]

-- | Stock default settings
defaultSettings:: Settings
defaultSettings = Settings
    { sVerticalSync = NoVSync
    , sWindowMode = Windowed
    }