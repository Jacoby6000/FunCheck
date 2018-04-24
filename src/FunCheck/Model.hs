{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module FunCheck.Model(Pattern(..), PatternF(..), Symbol) where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (foldl')
import Data.Functor.Identity

data Pattern a
  = Lit a
  | IntRange Int Int
  | DecRange Double Double
  | Optional (Pattern a)
  | Repeat (Pattern a) (Maybe Int) (Maybe Int)
  | And [Pattern a]
  | Or [Pattern a]
  | Var Symbol
  | Let Symbol (Pattern a)
  deriving(Show)

newtype Symbol = Symbol String
  deriving(Show)

makeBaseFunctor ''Pattern
