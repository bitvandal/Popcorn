-- | Resource management
module Popcorn.Engine.Managed.Extra
    ( -- * Monad managed extra functions
      bracketManaged
    ) where

import Control.Exception (bracket)
import Control.Monad.Managed (MonadManaged, managed)

-- | Bracket for Managed resources
bracketManaged :: MonadManaged m => IO a -> (a -> IO ()) -> m a
bracketManaged create destroy = managed (bracket create destroy)