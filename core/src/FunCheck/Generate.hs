{-# LANGUAGE TemplateHaskell      , FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module FunCheck.Generate(cataState, monoidalStepRandomGenAlgebra, Env(..)) where

import Control.Monad.State.Lazy
import Control.Lens
import Data.Foldable
import Data.Map.Lazy
import FunCheck.Data.Template
import System.Random
import Yaya.Control
import Yaya

data Env g a = Env { _randomGen :: g , _bindings :: Map Symbol a } deriving(Show, Eq)
makeLenses ''Env

instance HasGen g (Env g a) where
  generator = randomGen

cataState :: (Traversable m, Recursive f m, Monad n) => AlgebraM (StateT s n) m a -> s -> f -> n a
cataState alg s fm = evalStateT (cataM alg fm) s

monoidalStepRandomGenAlgebra
  :: (RandomGen g, Monoid a, MonadState (Env g a) m)
  => (Int -> a)
  -> (Double -> a)
  -> (String -> a)
  -> (t -> a)
  -> Template t a
  -> m a
monoidalStepRandomGenAlgebra _ _ _ f (Lit t          ) = pure $ f t
monoidalStepRandomGenAlgebra f _ _ _ (IntRange  mn mx) = f <$> randomRange (mn, mx)
monoidalStepRandomGenAlgebra _ f _ _ (DecRange  mn mx) = f <$> randomRange (mn, mx)
monoidalStepRandomGenAlgebra _ _ f _ (CharRange mn mx) = f . (: []) <$> randomRange (mn, mx)
monoidalStepRandomGenAlgebra _ _ _ _ (Optional a     ) = (\b -> if b then a else mempty) <$> (generator %%= random)
monoidalStepRandomGenAlgebra _ _ _ _ (Repeat a mn mx ) = foldMap id . flip replicate a <$> randomRange (mn, mx)
monoidalStepRandomGenAlgebra _ _ _ _ (And as         ) = pure $ fold as
monoidalStepRandomGenAlgebra _ _ _ _ (Or  as         ) = randomChoice as
monoidalStepRandomGenAlgebra _ _ _ _ (Var sym        ) = uses bindings (findWithDefault mempty sym)
monoidalStepRandomGenAlgebra _ _ _ _ (Let sym a      ) = mempty <$ (bindings %= (insert sym a))

randomRange :: (RandomGen g, Random r, HasGen g (Env g a), MonadState (Env g a) m) => (r, r) -> m r
randomRange x = generator %%= randomR x

randomChoice :: (RandomGen g, HasGen g (Env g a), MonadState (Env g a) m) => [r] -> m r
randomChoice rs = (!!) rs <$> randomRange (0, length rs - 1)

class HasGen g s | s -> g where
  generator :: Lens' s g
