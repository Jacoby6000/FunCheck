module FunCheck.Tests
  ( tests
  )
where

import           Control.Monad.State.Lazy
import           Distribution.TestSuite
import           FunCheck.Data.Gen
import           FunCheck.Data.Regex
import           FunCheck.Data.TemplateAlg
import           System.Random
import qualified Data.CharSet.Common           as CS
import           Data.CharSet
import           Data.Time.Clock.System

tests :: IO [Test]
tests = return [regexTests]


regexTests :: Test
regexTests = testGroup "Regex" [randomRegexTests]


randomRegexTests :: Test
randomRegexTests = testGroup "Random" randomTs
 where
  time :: IO Int
  time = fromIntegral . systemSeconds <$> getSystemTime

  testRandom :: String -> (Int -> Progress) -> TestInstance
  testRandom name f = test [regexTest, randomTest] name (f <$> time)

  conf = RandomOutputAlgConfig 0 10

  alg :: RandomGen g => RegularDataTemplateAlg (StateT g [])
  alg = randomOutputAlg conf


  evalExpecting :: (String -> a -> Progress) -> String -> a -> Int -> Progress
  evalExpecting compare regex expected time = errOr verifyResult builtState
   where
    builtState = tryProvideMatch alg CS.ascii regex
    verifyResult st = compare (evalStateT st (mkStdGen time)) expected

  literal = evalExpecting shouldEqual "a" "a"
  empty   = evalExpecting shouldEqual "^" "\0"

  lowerAlphaChar =
    let lowercaseChars = ((: []) <$> toList CS.lower) in evalExpecting shouldBeOneOf "[a-z]" lowercaseChars

  upperAlphaChar =
    let uppercaseChars = ((: []) <$> toList CS.upper) in evalExpecting shouldBeOneOf "[A-Z]" uppercaseChars

  randomTs :: [Test]
  randomTs =
    Test
      <$> [ testRandom "Literal"               literal
          , testRandom "Empty"                 empty
          , testRandom "Lowercase Alpha Char"  lowerAlphaChar
          , testRandom "Uppsercase Alpha Char" upperAlphaChar
          ]


test :: [String] -> String -> IO Progress -> TestInstance
test ts n r = TestInstance {run = r, name = n, tags = ts, options = [], setOption = \_ _ -> Right (test ts n r)}


shouldEqual :: (Eq a, Show a) => a -> a -> Progress
shouldEqual l r = if l == r then Finished Pass else failure (show l ++ " did not equal expected " ++ show r)

shouldBeOneOf :: (Eq a, Show a) => a -> [a] -> Progress
shouldBeOneOf a as = if a `elem` as then Finished Pass else failure (show a ++ " was not one of " ++ show as)

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
