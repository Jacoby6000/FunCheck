{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Data.TemplateAlg
  ( RegularDataTemplateAlg(..)
  )
where

import           Data.List.NonEmpty
import           Control.Applicative

data RegularDataTemplateAlg f = RegularDataTemplate {
  _repeat :: forall a. (Maybe Int, Maybe Int) -> f a -> f [a],
  _oneOf :: forall a. [f a] -> f a,
  _lit :: forall a. a -> f a,
  _chain :: forall a. f a -> f a -> f a
}

_plus :: Applicative f => RegularDataTemplateAlg f -> f a -> f (NonEmpty a)
_plus t fa = (:|) <$> fa <*> _repeat t (Nothing, Nothing) fa


_star :: RegularDataTemplateAlg f -> f a -> f [a]
_star t = _repeat t (Nothing, Nothing)
