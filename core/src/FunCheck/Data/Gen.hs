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

import           Control.Monad.State
import           Data.Bifunctor
import           Data.Functor.Plus
import           Data.Maybe
import           FunCheck.Data.Alg
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           System.Random

randLit :: RegularGrammarAlg f -> Gen a -> IO (f a)
randLit t g = lit t <$> generate g

arbLit :: Arbitrary a => RegularGrammarAlg f -> IO (f a)
arbLit t = randLit t arbitrary

randRange :: Random a => RegularGrammarAlg f -> (a, a) -> IO (f a)
randRange t r = randLit t (choose r)


data RandomOutputAlgConfig = RandomOutputAlgConfig {
  _minRepeat :: Int,
  _maxRepeat :: Int
}


randomOutputAlg :: forall g f
                 . (RandomGen g, MonadState g f, Plus f)
                => RandomOutputAlgConfig
                -> RegularGrammarAlg f
randomOutputAlg conf = RegularGrammarAlg
  { repeatN = repeatN'
  , oneOf   = oneOf'
  , lit     = pure
  , chain   = chain'
  }
 where
  chain' :: forall a . f a -> f a -> f a
  chain' = combined
   where
    splat = modify (snd . split)
    combined s1 s2 = (splat *> s1) <!> (splat *> s2)

  minRep :: Int
  minRep = _minRepeat conf

  maxRep :: Int
  maxRep = _maxRepeat conf

  repeatN' :: (Maybe Int, Maybe Int) -> f a -> f a
  repeatN' range fa =
    let rangeWithDefaults = bimap (fromMaybe minRep) (fromMaybe maxRep) range
    in  foldl chain' zero =<< (flip replicate fa <$> (state $ randomR rangeWithDefaults))

  oneOf' :: [f a] -> f a
  oneOf' fas = join (state $ randomPick fas)

randomPick :: RandomGen g => [a] -> g -> (a, g)
randomPick as g = first (as !!) (randomR (0, pred $ length as) g)
