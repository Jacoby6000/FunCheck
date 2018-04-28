{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable, DeriveAnyClass #-}

module FunCheck.Model(Template(..), Symbol) where

data Template a
  = Lit String
  | IntRange Int Int
  | DecRange Double Double
  | CharRange Char Char
  | Optional a
  | Repeat a Int Int
  | And [a]
  | Or [a]
  | Var Symbol
  | Let Symbol a
  deriving(Show, Functor, Traversable)

newtype Symbol = Symbol String
  deriving(Show, Eq, Ord)

instance Foldable Template where
  foldr _ z (Lit         _) = z
  foldr _ z (IntRange  _ _) = z
  foldr _ z (DecRange  _ _) = z
  foldr _ z (CharRange _ _) = z
  foldr f z (Optional    a) = f a z
  foldr f z (Repeat  a _ _) = f a z
  foldr f z (And        as) = foldr f z as
  foldr f z (Or         as) = foldr f z as
  foldr _ z (Var         _) = z
  foldr f z (Let _       a) = f a z


