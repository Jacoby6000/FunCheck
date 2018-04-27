module FunCheck.Generate(cataState) where

import Control.Monad.State.Lazy
import Yaya.Control

cataState :: (Traversable m, Recursive f m) => (m a -> State g a) -> g -> f -> a
cataState alg gen fm = evalState (cataM alg fm) gen


