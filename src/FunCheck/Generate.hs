module FunCheck.Generate(cataState, monoidalTemplateState, Env(..)) where

import Control.Monad.State.Lazy
import System.Random
import Yaya.Control
import Yaya
import FunCheck.Model
import Data.Map.Lazy
import Data.Bifunctor

data Env g a = Env { randomGen :: g
                   , bindings :: Map Symbol a
                   }

cataState :: (Traversable m, Recursive f m) => AlgebraM (State s) m a -> s -> f -> a
cataState alg s fm = evalState (cataM alg fm) s

monoidalTemplateState :: (RandomGen g, Monoid a) =>
                         (Int    -> a) ->
                         (Double -> a) ->
                         (String -> a) ->
                         Template a    ->
                         State (Env g a) a
monoidalTemplateState a b c d = state $ monoidalStepAlgebra a b c d

monoidalStepAlgebra :: (RandomGen g, Monoid a) =>
                       (Int    -> a) ->
                       (Double -> a) ->
                       (String -> a) ->
                       Template a    ->
                       Env g a       ->
                       (a, Env g a)
monoidalStepAlgebra _ _ f (Lit           s) = (,) (f s)
monoidalStepAlgebra f _ _ (IntRange  mn mx) = first f . randomRange mn mx
monoidalStepAlgebra _ f _ (DecRange  mn mx) = first f . randomRange mn mx
monoidalStepAlgebra _ _ f (CharRange mn mx) = first (f . (:[])) . randomRange mn mx
monoidalStepAlgebra _ _ _ (Optional      a) = first (\b -> if b then a else mempty) . randomResult
monoidalStepAlgebra _ _ _ (Repeat  a mn mx) = first (\n -> foldMap id (replicate n a)) . randomRange mn mx
monoidalStepAlgebra _ _ _ (And          as) = (,) $ foldMap id as
monoidalStepAlgebra _ _ _ (Or           as) = randomChoice as
monoidalStepAlgebra _ _ _ (Var         sym) = fetchBinding sym mempty
monoidalStepAlgebra _ _ _ (Let     sym   a) = (,) mempty . addBinding sym a

randomRange :: (RandomGen g, Random r) => r -> r -> Env g a -> (r, Env g a)
randomRange mn mx env = updateEnvGen (randomR (mn, mx) (randomGen env)) env

randomResult :: (RandomGen g, Random r) => Env g a -> (r, Env g a)
randomResult env = updateEnvGen (random $ randomGen env) env

randomChoice :: RandomGen g => [r] -> Env g a -> (r, Env g a)
randomChoice rs env = first (rs !!) (randomRange 0 (length rs -1) env)

updateEnvGen :: (r, g) -> Env g a -> (r, Env g a)
updateEnvGen (r, g) (Env _ a) = (r, Env g a)

fetchBinding :: Symbol -> a -> Env g a -> (a, Env g a)
fetchBinding sym a env = (findWithDefault a sym $ bindings env, env)

addBinding :: Symbol -> a -> Env g a -> Env g a
addBinding k v e = Env (randomGen e) (insert k v $ bindings e)
