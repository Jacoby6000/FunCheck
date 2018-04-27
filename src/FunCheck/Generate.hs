module FunCheck.Generate(generateData) where

import System.Random
import FunCheck.Model
import Control.Monad.State.Lazy
import Yaya.Control

-- Template a -> State g a
generateData :: (Traversable m, Recursive f m, RandomGen g) => (m a -> State g a) -> g -> f -> a
generateData alg gen template = fst (runState (cataM alg template) gen)
