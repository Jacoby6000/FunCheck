{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Data.TemplateAlg
  ( RegularDataTemplateAlg(..)
  , plus
  , star
  , chainAll
  , optional
  )
where

import           Data.Semigroup

data RegularDataTemplateAlg f = RegularDataTemplate {
  repeatN :: forall a. (Maybe Int, Maybe Int) -> f a -> f a,
  oneOf :: forall a. [f a] -> f a,
  lit :: forall a. a -> f a,
  chain :: forall a. f a -> f a -> f a
}

plus :: (Applicative f, Semigroup a) => RegularDataTemplateAlg f -> f a -> f a
plus t fa = (<>) <$> fa <*> repeatN t (Nothing, Nothing) fa

optional :: (Functor f) => RegularDataTemplateAlg f -> f a -> f a
optional t = repeatN t (Just 0, Just 1)

star :: RegularDataTemplateAlg f -> f a -> f a
star t = repeatN t (Nothing, Nothing)

chainAll :: RegularDataTemplateAlg f -> f a -> [f a] -> f a
chainAll t = foldl (chain t)
