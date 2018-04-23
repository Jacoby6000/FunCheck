module FunCheck.Data.Model(Pattern(..), Symbol) where

data Pattern a
  = Lit String
  | IntRange Int Int
  | DecRange Double Double
  | Optional a
  | Repeat a (Maybe Int) (Maybe Int)
  | And [a]
  | Or [a]
  | Var Symbol
  | Let Symbol a

newtype Symbol = Symbol String
