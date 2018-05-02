module FunCheck.Parse.RegexSpec(main, spec) where

import Test.Hspec
import FunCheck.Parse.Regex
import Data.Attoparsec.Text
import Data.Text

main :: IO ()
main = hspec spec

spec = do
  describe "Regex.regex" $ do
    it "Can parse a single Literal" $ do
      testParser regex "a" `shouldBe` (Right $ Lit "a")
    it "Can parse a StartAnchor" $ do
      testParser regex "^" `shouldBe` (Right $ StartAnchor)
    it "Can parse an EndAnchor" $ do
      testParser regex "$" `shouldBe` (Right $ EndAnchor)








testParser :: Parser a -> String -> Either String a
testParser parser s = eitherResult $ parse parser (pack s)
