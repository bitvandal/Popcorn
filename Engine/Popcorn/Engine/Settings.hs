-- | Engine settings
module Popcorn.Engine.Settings
    ( -- * Data types
      Settings(..)
    , VerticalSyncMode(..)

    -- * Stock configurations
    , defaultSettings
    ) where

-- | Engine settings
newtype Settings = Settings
    { sVerticalSync :: VerticalSyncMode
    } deriving stock Show

-- | V-Sync modes
data VerticalSyncMode =
      NoVSync
    | WhatVSyncModesDoIWant
    deriving stock (Eq, Show)

-- | Stock default settings
defaultSettings:: Settings
defaultSettings = Settings
    { sVerticalSync = NoVSync
    }