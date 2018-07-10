module FunCheck.Tests
  ( tests
  )
where

import           Control.Monad.List
import           Data.CharSet
import           Distribution.TestSuite
import           FunCheck.Data.Gen
import           FunCheck.Data.Regex
import           FunCheck.Data.TemplateAlg
import qualified Data.CharSet.Common           as CS

tests :: IO [Test]
tests = return [regexTests]


regexTests :: Test
regexTests = testGroup "Regex" [randomRegexTests]


randomRegexTests :: Test
randomRegexTests = testGroup "Random" randomTs
 where
  testRandom :: String -> IO Progress -> TestInstance
  testRandom = test [regexTest, randomTest]

  alg :: RegularDataTemplateAlg (ListT IO)
  alg = randomOutputAlg $ RandomOutputAlgConfig 0 10

  evalExpecting :: (String -> a -> Progress) -> String -> a -> IO Progress
  evalExpecting f regex expected = errOr (`f` expected) <$> builtResult
   where
    builtResult = sequence (runListT <$> tryProvideMatch alg CS.ascii regex)

  literal  = evalExpecting shouldEqual "a" "a"
  emptyPat = evalExpecting shouldEqual "^" "\0"
  combineExprs = evalExpecting shouldEqual "abcd" "abcd"

  lowerAlphaChar =
    let lowercaseChars = ((: []) <$> toList CS.lower)
    in  evalExpecting shouldBeOneOf "[a-z]" lowercaseChars

  upperAlphaChar =
    let uppercaseChars = ((: []) <$> toList CS.upper)
    in  evalExpecting shouldBeOneOf "[A-Z]" uppercaseChars

  randomTs :: [Test]
  randomTs =
    Test
      <$> [ testRandom "Literal"               literal
          , testRandom "Empty"                 emptyPat
          , testRandom "Combine Expressions"   combineExprs
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
