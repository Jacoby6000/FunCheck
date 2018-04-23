module FunCheck.Generate where

import FunCheck.Model
import Control.Monad.State
import Data.Functor.Foldable

generateData :: Pattern a -> (PatternF a b -> b) -> b
generateData pat alg = cata alg pat

