{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Data.Gen
  ( randLit
  , arbLit
  , randRange
  , randomOutputAlg
  , randomPick
  , RandomOutputAlgConfig(..)
  )
where

import           System.Random

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Functor.Alt
import           Data.Maybe

import           FunCheck.Data.TemplateAlg



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

randomOutputAlg :: (MonadState g f, RandomGen g, Alt f)
                => RandomOutputAlgConfig
                -> RegularDataTemplateAlg f
randomOutputAlg conf = RegularDataTemplate
  { repeatN = repeatN'
  , oneOf   = oneOf'
  , lit     = pure
  , chain   = (<!>)
  }
 where

  minRep :: Int
  minRep = _minRepeat conf

  maxRep :: Int
  maxRep = _maxRepeat conf

  repeatN' :: (MonadState g f, RandomGen g)
           => (Maybe Int, Maybe Int)
           -> f a
           -> f [a]
  repeatN' range fa =
    let rangeWithDefaults = bimap (fromMaybe minRep) (fromMaybe maxRep) range
    in  state (randomR rangeWithDefaults) >>= flip replicateM fa

  oneOf' :: (MonadState g f, RandomGen g) => [f a] -> f a
  oneOf' fas = join (state $ randomPick fas)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick as g = first (as !!) (randomR (0, pred $ length as) g)
