module Main where

import           FunCheck.Data.Template
import           FunCheck.Generate
import           System.Random
import           Data.Map.Lazy
import           Yaya.Unsafe.Data

main :: IO ()
main = do
  gen    <- getStdGen
  result <- cataState (monoidalStepRandomGenAlgebra show show id id)
                      (Env gen empty)
                      template
  putStrLn result

template :: Fix (Template String)
template
  = Fix $ And
      (Fix (Lit "foo"))
      (Fix $ And
        (Fix
          (Let (Symbol "numOrChar")
               (Fix (Or [Fix $ IntRange 10 20, Fix $ CharRange 'a' 'z']))
          )
        )
        (Fix $ And
          (Fix (Var (Symbol "numOrChar")))
          (Fix $ And
            (Fix (Var (Symbol "numOrChar")))
            (Fix $ And
              (Fix (Var (Symbol "numOrChar")))
              (Fix $ And
                (Fix (Var (Symbol "numOrChar")))
                (Fix $ And
                  (Fix (Var (Symbol "numOrChar")))
                  (Fix $ And
                    (Fix (Lit "\n1: "))
                    (Fix $ And
                      (Fix (IntRange 10 20))
                      (Fix $ And
                        (Fix (Lit "\n2: "))
                        (Fix $ And
                          (Fix (IntRange 10 20))
                          (Fix $ And
                            (Fix (Lit "\n3: "))
                            (Fix $ And
                              (Fix (IntRange 10 20))
                              (Fix $ And
                                (Fix (Lit "\n4: "))
                                (Fix $ And
                                  (Fix (IntRange 10 20))
                                  (Fix $ And (Fix (Lit "\n5: "))
                                             (Fix $ IntRange 10 20)
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
    )
              )
            )
          )
        )
