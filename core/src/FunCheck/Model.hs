{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable, DeriveFoldable #-}

module FunCheck.Model(Template(..), Symbol(..)) where

data Template t a
  = Lit t
  | IntRange Int Int
  | DecRange Double Double
  | CharRange Char Char
  | Optional a
  | Repeat a Int Int
  | And [a]
  | Or [a]
  | Var Symbol
  | Let Symbol a
  deriving(Show, Functor, Foldable, Traversable)

newtype Symbol = Symbol String
  deriving(Show, Eq, Ord)


