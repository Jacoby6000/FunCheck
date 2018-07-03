module FunCheck.Tests
  ( tests
  )
where

import           Distribution.TestSuite

tests :: IO [Test]
tests = return [Test stuff]
 where
  stuff = TestInstance
    { run       = return $ Finished Pass
    , name      = "stuff"
    , tags      = []
    , options   = []
    , setOption = \_ _ -> Right stuff
    }
