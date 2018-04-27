{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}

module FunCheck.Model(Template(..), Symbol) where

import Data.List (foldl')
import Data.Functor.Identity



data Template a
  = Lit String
  | IntRange Int Int
  | DecRange Double Double
  | Optional a
  | Repeat a (Maybe Int) (Maybe Int)
  | And [a]
  | Or [a]
  | Var Symbol
  | Let Symbol a
  deriving(Show)


newtype Symbol = Symbol String
  deriving(Show)

instance Functor Template where
  fmap _ (Lit          s) = Lit s
  fmap _ (IntRange mn mx) = IntRange mn mx
  fmap _ (DecRange mn mx) = DecRange mn mx
  fmap f (Optional     a) = Optional (f a)
  fmap f (Repeat a mn mx) = Repeat (f a) mn mx
  fmap f (And         as) = And $ f <$> as
  fmap f (Or          as) = Or $ f <$> as
  fmap _ (Var          s) = Var s
  fmap f (Let sym      a) = Let sym (f a)


instance Foldable Template where
  foldr _ z (Lit        _) = z
  foldr _ z (IntRange _ _) = z
  foldr _ z (DecRange _ _) = z
  foldr f z (Optional   a) = f a z
  foldr f z (Repeat a _ _) = f a z
  foldr f z (And         as) = foldr f z as
  foldr f z (Or          as) = foldr f z as
  foldr _ z (Var          s) = z
  foldr f z (Let sym      a) = f a z


  foldMap f = foldr (\a b -> mappend (f a) b) mempty
