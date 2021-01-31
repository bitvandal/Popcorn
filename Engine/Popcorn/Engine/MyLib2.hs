-- | Other module documentation
module Popcorn.Engine.MyLib2 ( someFunc2 ) where

import qualified Popcorn.Common.MyLib as Common

-- | Uses "Popcorn.Common.MyLib" module
someFunc2 :: IO ()
someFunc2 = Common.someFunc
