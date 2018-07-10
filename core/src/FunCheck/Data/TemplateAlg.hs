{-# LANGUAGE TypeOperators, RankNTypes #-}

module FunCheck.Data.TemplateAlg
  ( RegularDataTemplateAlg(..)
  , plus
  , star
  , chainAll
  , optional
  )
where

data RegularDataTemplateAlg f = RegularDataTemplate {
  repeatN :: forall a. (Maybe Int, Maybe Int) -> f a -> f a,
  oneOf :: forall a. [f a] -> f a,
  lit :: forall a. a -> f a,
  chain :: forall a. f a -> f a -> f a
}

plus :: (Applicative f) => RegularDataTemplateAlg f -> f a -> f a
plus = flip repeatN (Just 1, Nothing)

optional :: (Functor f) => RegularDataTemplateAlg f -> f a -> f a
optional = flip repeatN (Just 0, Just 1)

star :: RegularDataTemplateAlg f -> f a -> f a
star = flip repeatN (Nothing, Nothing)

chainAll :: RegularDataTemplateAlg f -> f a -> [f a] -> f a

chainAll t = foldl (chain t)
