-- | Supplementary Monadic functions
module Popcorn.Common.Control.Monad.Extra
    ( -- * Monadic functions
      findM
    ) where

-- | A monadic find apt for instances of Foldable
findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM cond = foldr f (pure Nothing)
  where
    f x acc = cond x >>= \y -> if y then pure (Just x) else acc