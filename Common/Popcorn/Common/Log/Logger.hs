-- | Logging system
module Popcorn.Common.Log.Logger
    ( -- * Types
      LogComponent(..)

      -- * Loggers
    , info
    , clientLog
    , engineLog
    , platformLog
    ) where

import qualified Data.Text as T

-- | Logger Components
data LogComponent =
      Platform -- ^ Platform component
    | Renderer -- ^ Renderer component
    | Engine   -- ^ The Engine
    | Client   -- ^ The Client
    deriving stock (Eq, Show)

-- | Log a informative message
info :: LogComponent -> T.Text -> IO ()
info component msg = putStrLn $ mconcat
    [ "["
    , show component
    , "] "
    , T.unpack msg
    ]

-- | Helper function to be used by the Clients to log a message
clientLog :: T.Text -> IO ()
clientLog = info Client

-- | Helper function to be used by The Engine to log a message
engineLog :: T.Text -> IO ()
engineLog = info Engine

-- | Helper function to be used by The Platform to log a message
platformLog :: T.Text -> IO ()
platformLog = info Platform