module Main where

import FunCheck.Model
import FunCheck.Generate
import System.Random
import Data.Map.Lazy
import Yaya.Unsafe.Data

main :: IO ()
main = do
  gen    <- getStdGen
  result <- cataState (monoidalStepAlgebra show show id id) (Env gen empty) template
  putStrLn result

template :: Fix (Template String)
template = Fix
  (And
    [ Fix (Lit "foo")
    , Fix (Let (Symbol "numOrChar") (Fix (Or [Fix $ IntRange 10 20, Fix $ CharRange 'a' 'z'])))
    , Fix (Var (Symbol "numOrChar"))
    , Fix (Var (Symbol "numOrChar"))
    , Fix (Var (Symbol "numOrChar"))
    , Fix (Var (Symbol "numOrChar"))
    , Fix (Var (Symbol "numOrChar"))
    , Fix (Lit "\n1: ")
    , Fix (IntRange 10 20)
    , Fix (Lit "\n2: ")
    , Fix (IntRange 10 20)
    , Fix (Lit "\n3: ")
    , Fix (IntRange 10 20)
    , Fix (Lit "\n4: ")
    , Fix (IntRange 10 20)
    , Fix (Lit "\n5: ")
    , Fix (IntRange 10 20)
    ]
  )

