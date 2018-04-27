{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass #-}

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
  deriving(Show, Functor, Traversable)

newtype Symbol = Symbol String
  deriving(Show)

instance Foldable Template where
  foldr _ z (Lit        _) = z
  foldr _ z (IntRange _ _) = z
  foldr _ z (DecRange _ _) = z
  foldr f z (Optional   a) = f a z
  foldr f z (Repeat a _ _) = f a z
  foldr f z (And       as) = foldr f z as
  foldr f z (Or        as) = foldr f z as
  foldr _ z (Var        s) = z
  foldr f z (Let sym    a) = f a z


