{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable, DeriveFoldable #-}

module FunCheck.Data.Template
  ( Template(..)
  , Symbol(..)
  )
    where

import Data.List.NonEmpty
import Control.Applicative

data RegexTemplate f = RegexTemplate {
  _repeat :: forall a. (Maybe Int, Maybe Int) -> f a -> f [a],
  _oneOf :: forall a. [f a] -> f a,
  _lit :: forall a. a -> f a,
  _chain :: forall a. f a -> f a -> f a
}

plus :: Applicative f => f a -> RegexTemplate f -> f (NonEmpty a)
plus fa t = (:|) <$> fa <*> _repeat t (Nothing, Nothing) fa


star :: f a -> RegexTemplate f -> f [a]
star fa t = _repeat t (Nothing, Nothing) fa


newtype Symbol = Symbol String
  deriving(Show, Eq, Ord)
