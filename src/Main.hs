module Main where

import FunCheck.Model
import FunCheck.Generate
import System.Random
import Data.Map.Lazy
import Yaya.Unsafe.Data

main :: IO ()
main = do { gen <- getStdGen
          ; putStrLn $ cataState (monoidalTemplateState show show id) (Env gen empty) template
          }

template :: Fix Template
template =
  (Fix (
    And [
      Fix (Lit "foo"),
      Fix (Or [Fix $ IntRange 10 20, Fix $ CharRange 'a' 'z'])
    ]
  ))
