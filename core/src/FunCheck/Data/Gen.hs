{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections #-}

module FunCheck.Data.Gen
  ( randLit
  , arbLit
  , randRange
  , randomOutputAlg
  , randomPick
  , RandomOutputAlgConfig(..)
  )
where

import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Functor.Plus
import           Data.Maybe
import           Data.Tuple
import           FunCheck.Data.TemplateAlg
import           System.Random
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

randLit :: RegularDataTemplateAlg f -> Gen a -> IO (f a)
randLit t g = lit t <$> generate g

arbLit :: Arbitrary a => RegularDataTemplateAlg f -> IO (f a)
arbLit t = randLit t arbitrary

randRange :: Random a => RegularDataTemplateAlg f -> (a, a) -> IO (f a)
randRange t r = randLit t (choose r)


data RandomOutputAlgConfig = RandomOutputAlgConfig {
  _minRepeat :: Int,
  _maxRepeat :: Int
}

randomOutputAlg :: forall g f
                 . (MonadState g f, RandomGen g, Plus f)
                => RandomOutputAlgConfig
                -> RegularDataTemplateAlg f
randomOutputAlg conf = RegularDataTemplate {repeatN = repeatN', oneOf = oneOf', lit = pure, chain = chain'}
 where


  chain' :: forall a. f a -> f a -> f a
  chain' = combined
    where
     splat = modify (snd . split)
     combined s1 s2 = (splat *> s1) <!> (splat *> s2)

  minRep :: Int
  minRep = _minRepeat conf

  maxRep :: Int
  maxRep = _maxRepeat conf

  repeatN' :: (Maybe Int, Maybe Int) -> f a -> f a
  repeatN' range = repeated
   where
    rangeWithDefaults = bimap (fromMaybe minRep) (fromMaybe maxRep) range
    repeated fa = foldl chain' zero =<< (flip replicate fa <$> state (randomR rangeWithDefaults))


  oneOf' :: (MonadState g f, RandomGen g) => [f a] -> f a
  oneOf' fas = join (state $ randomPick fas)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick as g = first (as !!) (randomR (0, pred $ length as) g)
