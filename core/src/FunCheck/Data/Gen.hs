{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module FunCheck.Data.Gen
  ( randLit
  , arbLit
  , randRange
  , randomOutputAlg
  , RandomOutputAlgConfig(..)
  )
where

import           Control.Monad.Random
import           Data.Bifunctor
import           Data.Functor.Plus
import           Data.Maybe
import           FunCheck.Data.TemplateAlg
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
                 . (RandomGen g, MonadRandom f, MonadSplit g f, Plus f)
                => RandomOutputAlgConfig
                -> RegularDataTemplateAlg f
randomOutputAlg conf = RegularDataTemplate
  { repeatN = repeatN'
  , oneOf   = oneOf'
  , lit     = pure
  , chain   = chain'
  }
 where
  chain' :: forall a . f a -> f a -> f a
  chain' = combined where combined s1 s2 = (getSplit *> s1) <!> (getSplit *> s2)

  minRep :: Int
  minRep = _minRepeat conf

  maxRep :: Int
  maxRep = _maxRepeat conf

  repeatN' :: (Maybe Int, Maybe Int) -> f a -> f a
  repeatN' range fa =
    let rangeWithDefaults = bimap (fromMaybe minRep) (fromMaybe maxRep) range
     in foldl chain' zero =<< (flip replicate fa <$> getRandomR rangeWithDefaults)

  oneOf' :: (MonadRandom f, RandomGen g) => [f a] -> f a
  oneOf' fas = join (uniform fas)
