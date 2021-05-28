-- | Miscellaneous utilities.
module Popcorn.Common.Utils
    ( maybeToEither
    ) where

-- | Converts a Maybe value into a Either value
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither leftValue = maybe (Left leftValue) Right