{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Parse.RegexSpec(main, spec) where

import Test.Hspec
import FunCheck.Parse.Regex
import Text.Parsec
import Data.Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Regex.regex" $ do
    context "Smoke tests" $ do
      it "Can parse a single Literal" $ do
        testParser regex "a" `shouldBe` (Right $ Lit "a")
      it "Can parse a StartAnchor" $ do
        testParser regex "^" `shouldBe` (Right $ StartAnchor)
      it "Can parse an EndAnchor" $ do
        testParser regex "$" `shouldBe` (Right $ EndAnchor)
      it "Can parse a OneOf" $ do
        testParser regex "[x]" `shouldBe` (Right $ OneOf [ChooseOneChar 'x'])
      it "Can parse a NotOneOf" $ do
        testParser regex "[^x]" `shouldBe` (Right $ NotOneOf [ChooseOneChar 'x'])
      it "Can parse a CaptureGroup" $ do
        testParser regex "(x)" `shouldBe` (Right $ CaptureGroup (Lit "x"))
      it "Can parse a Special" $ do
        testParser regex "\\b" `shouldBe` (Right $ Special Backspace)

testParser :: Parsec String () a -> String -> Either ParseError a
testParser parser s = parse parser "test" s

