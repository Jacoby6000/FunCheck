module FunCheck.Tests.Regex (regexTests) where

import Control.Monad.State.Lazy
import Data.CharSet
import Distribution.TestSuite
import FunCheck.Data.Alg
import FunCheck.Data.Gen
import FunCheck.Data.Regex
import FunCheck.Tests.Util
import System.Random
import qualified Data.CharSet.Common as CS


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

  checkEquals          = evalExpecting shouldEqual
  checkOneOf           = evalExpecting shouldBeOneOf
  checkContainsOnly    = evalExpecting containsOnly

  allChars             = CS.ascii
  lowercaseChars       = CS.lower
  uppercaseChars       = CS.upper

  alphaChars           = lowercaseChars `union` uppercaseChars

  nonAlphaChars        = Data.CharSet.filter (`notMember` alphaChars) allChars

  lowercaseCharStrings = pure <$> toList lowercaseChars
  uppercaseCharStrings = pure <$> toList uppercaseChars
  nonAlphaCharsList    = toList nonAlphaChars

  randomTs :: [Test]
  randomTs =
    Test
      <$> [ testRandom "Literal" $ "a" `checkEquals` "a"
          , testRandom "Empty" $ "^" `checkEquals` "\0"
          , testRandom "Combine Literals" $ "abcd" `checkEquals` "abcd"
          , testRandom "Lowercase Alpha Char" $ "[a-z]" `checkOneOf` lowercaseCharStrings
          , testRandom "Uppercase Alpha Char" $ "[A-Z]" `checkOneOf` uppercaseCharStrings
          , testRandom "Numeric Range" $ "[0-5]" `checkOneOf` ["0", "1", "2", "3", "4", "5"]
          , testRandom "Expr Plus" $ "[^a-zA-Z]+" `checkContainsOnly` nonAlphaCharsList
          ]


regexTest :: String
regexTest = "regex"
