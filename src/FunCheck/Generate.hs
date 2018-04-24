module FunCheck.Generate where

import FunCheck.Model
import Control.Monad.State
import Data.Functor.Foldable



generateData :: Pattern a -> (PatternF String a -> a) -> a
generateData a alg = cata alg a


