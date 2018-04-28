{-# LANGUAGE TemplateHaskell      , FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module FunCheck.Generate(cataState, monoidalStepAlgebra, Env(..)) where

import Control.Monad.State.Lazy
import System.Random
import Yaya.Control
import Yaya
import FunCheck.Model
import Data.Map.Lazy
import Control.Lens

data Env g a = Env { _randomGen :: g , _bindings :: Map Symbol a } deriving(Show, Eq)
makeLenses ''Env

instance HasGen g (Env g a) where
  generator = randomGen

cataState :: (Traversable m, Recursive f m, Monad n) => AlgebraM (StateT s n) m a -> s -> f -> n a
cataState alg s fm = evalStateT (cataM alg fm) s

monoidalStepAlgebra :: (RandomGen g, Monoid a, MonadState (Env g a) m) =>
                       (Int    -> a) ->
                       (Double -> a) ->
                       (String -> a) ->
                       (t      -> a) ->
                       Template t a  ->
                       m a
monoidalStepAlgebra _ _ _ f (Lit           t) = state $ (,) (f t)
monoidalStepAlgebra f _ _ _ (IntRange  mn mx) = f <$> randomRange (mn, mx)
monoidalStepAlgebra _ f _ _ (DecRange  mn mx) = f <$> randomRange (mn, mx)
monoidalStepAlgebra _ _ f _ (CharRange mn mx) = f . (:[]) <$> randomRange (mn, mx)
monoidalStepAlgebra _ _ _ _ (Optional      a) = (\b -> if b then a else mempty) <$> randomResult
monoidalStepAlgebra _ _ _ _ (Repeat  a mn mx) = foldMap id <$> flip replicate a <$> randomRange (mn, mx)
monoidalStepAlgebra _ _ _ _ (And          as) = state $ (,) (foldMap id as)
monoidalStepAlgebra _ _ _ _ (Or           as) = randomChoice as
monoidalStepAlgebra _ _ _ _ (Var         sym) = state $ preserveInput (findWithDefault mempty sym . (view bindings))
monoidalStepAlgebra _ _ _ _ (Let     sym   a) = state (\env -> (mempty, over bindings (insert sym a) env))


preserveInput :: (a -> b) -> a -> (b, a)
preserveInput f a = (f a, a)

randomRange :: (RandomGen g, Random r, HasGen g (Env g a), MonadState (Env g a) m) => (r, r) -> m r
randomRange x = generator %%= randomR x

randomResult :: (RandomGen g, Random r, HasGen g (Env g a), MonadState (Env g a) m) => m r
randomResult = generator %%= random

randomChoice :: (RandomGen g, HasGen g (Env g a), MonadState (Env g a) m) => [r] -> m r
randomChoice rs = (!!) rs <$> randomRange (0, length rs - 1)

withMonadState :: MonadState s m => (s -> s) -> m a -> m a
withMonadState f m = modify f >> m

class HasGen g s | s -> g where
  generator :: Lens' s g
