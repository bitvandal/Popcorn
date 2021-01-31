module Popcorn.Engine.MyLib2 ( someFunc ) where

import qualified Popcorn.Common.MyLib

someFunc :: IO ()
someFunc = Popcorn.Common.MyLib.someFunc
