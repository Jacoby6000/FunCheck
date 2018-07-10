module FunCheck.Tests
  ( tests
  )
where

import           Control.Monad.State.Lazy
import           Data.CharSet
import           Data.Time.Clock.System
import           Distribution.TestSuite
import           FunCheck.Data.Alg
import           FunCheck.Data.Gen
import           FunCheck.Data.Regex
import           System.Random
import qualified Data.CharSet.Common           as CS

tests :: IO [Test]
tests = return [regexTests]

regexTests :: Test
regexTests = testGroup "Regex" [randomRegexTests]

randomRegexTests :: Test
randomRegexTests = testGroup "Random" randomTs
 where
  testRandom :: String -> (Int -> Progress) -> TestInstance
  testRandom n f = test [regexTest, randomTest] n (f <$> time)

  alg :: RandomGen g => RegularGrammarAlg (StateT g [])
  alg = randomOutputAlg $ RandomOutputAlgConfig 0 10

  evalExpecting :: (String -> a -> Progress) -> String -> a -> Int -> Progress
  evalExpecting f regex expected seed = errOr verifyResult builtState
   where
    builtState = tryProvideMatch alg CS.ascii regex
    verifyResult st = f (evalStateT st (mkStdGen seed)) expected

  checkEquals    = evalExpecting shouldEqual
  checkOneOf     = evalExpecting shouldBeOneOf
  lowercaseChars = (: []) <$> toList CS.lower
  uppercaseChars = (: []) <$> toList CS.upper

  randomTs :: [Test]
  randomTs =
    Test
      <$> [ testRandom "Literal"               $ "a"     `checkEquals` "a"
          , testRandom "Empty"                 $ "^"     `checkEquals` "\0"
          , testRandom "Combine Literals"      $ "abcd"  `checkEquals` "abcd"
          , testRandom "Lowercase Alpha Char"  $ "[a-z]" `checkOneOf`  lowercaseChars
          , testRandom "Uppsercase Alpha Char" $ "[A-Z]" `checkOneOf`  uppercaseChars
          ]

time :: IO Int
time = fromIntegral . systemSeconds <$> getSystemTime

test :: [String] -> String -> IO Progress -> TestInstance
test ts n r =
  TestInstance {run = r, name = n, tags = ts, options = [], setOption = \_ _ -> Right (test ts n r)}

shouldEqual :: (Eq a, Show a) => a -> a -> Progress
shouldEqual l r =
  if l == r then Finished Pass else failure (show l ++ " did not equal expected " ++ show r)

shouldBeOneOf :: (Eq a, Show a) => a -> [a] -> Progress
shouldBeOneOf a as =
  if a `elem` as then Finished Pass else failure (show a ++ " was not one of " ++ show as)

errOr :: Show e => (a -> Progress) -> Either e a -> Progress
errOr = either (err . show)

err :: String -> Progress
err = Finished . Error

failure :: String -> Progress
failure = Finished . Fail


regexTest :: String
regexTest = "regex"

randomTest :: String
randomTest = "regex"
