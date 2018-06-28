{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Data.TemplateAlg
  ( RegularDataTemplateAlg(..)
  , plus
  , star
  )
where

import           Data.List.NonEmpty

data RegularDataTemplateAlg f = RegularDataTemplate {
  repeatN :: forall a. (Maybe Int, Maybe Int) -> f a -> f [a],
  oneOf :: forall a. [f a] -> f a,
  lit :: forall a. a -> f a
}

plus :: Applicative f => RegularDataTemplateAlg f -> f a -> f (NonEmpty a)
plus t fa = (:|) <$> fa <*> repeatN t (Nothing, Nothing) fa

star :: RegularDataTemplateAlg f -> f a -> f [a]
star t = repeatN t (Nothing, Nothing)
