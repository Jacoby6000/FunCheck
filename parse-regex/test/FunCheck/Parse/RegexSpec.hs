module FunCheck.Parse.RegexSpec(main, spec) where

import Test.Hspec
import FunCheck.Parse.Regex
import Data.Attoparsec.Text
import Data.Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Regex.regex" $ do
    context "Smoke tests" $ do
      it "Can parse a single Literal" $ do
        testParser regex "a" `shouldBe` (Right $ Lit 'a')
      it "Can parse a StartAnchor" $ do
        testParser regex "^" `shouldBe` (Right $ StartAnchor)
      it "Can parse an EndAnchor" $ do
        testParser regex "$" `shouldBe` (Right $ EndAnchor)
      it "Can parse a OneOf" $ do
        testParser regex "[x]" `shouldBe` (Right $ OneOf [ChooseOneChar 'x'])
      it "Can parse a NotOneOf" $ do
        testParser regex "[^x]" `shouldBe` (Right $ NotOneOf [ChooseOneChar 'x'])
      it "Can parse a CaptureGroup" $ do
        testParser regex "(x)" `shouldBe` (Right $ CaptureGroup [(Lit 'x')])


testParser :: Parser a -> String -> Either String a
testParser parser s = parseOnly parser (pack s)
