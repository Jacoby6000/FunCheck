module FunCheck.Parse.Regex(Regex(..), Choose(..), SpecialChar(..), specialChar, choose, regex) where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import Data.Char
import Data.Functor

import Data.Attoparsec.Text
import qualified Data.Text as T

data Regex
  = Lit Char
  | StartAnchor
  | EndAnchor
  | OneOf [Choose]
  | NotOneOf [Choose]
  | CaptureGroup [Regex]
  | Special SpecialChar
  | Optional Regex
  | Or Regex Regex
  | And [Regex]
  deriving(Show, Eq)

data Choose
 = ChooseOneChar Char
 | ChooseCharRange Char Char
 | ChooseSpecialChar SpecialChar
  deriving(Show, Eq)

data SpecialChar
 = Whitespace
 | Number
 | AlphaNumeric
 | Slash
 | Tab
 | VerticalTab
 | CR
 | LF
 | CRLF
 | Escape
 | Backspace
 | FormFeed
 | Alert
 | NullEscape
 | OctalEscape Char
 | HexadecimalEscape Char
 | EscapedChar Char
  deriving(Show, Eq)

checkSeperator :: Parser ()
checkSeperator = endOfInput

regex :: Parser Regex
regex = combine <$> many1 parseRegexAtoms
 where
  combine (h:[]) = h
  combine (h:t) = And (h:t)

parseRegexAtoms :: Parser Regex
parseRegexAtoms = startAnchor <|> endAnchor <|> notOneOf <|> oneOf <|> captureGroup <|> special <|> lit
 where
  notOneOf = NotOneOf <$> parseAllWithin "[^" choose "]"
  oneOf = OneOf <$> parseAllWithin "[" choose "]"
  captureGroup = CaptureGroup <$> parseAllWithin "(" regex ")"
  special = Special <$> specialChar
  lit = Lit <$> anyChar
  startAnchor = char '^' $> StartAnchor
  endAnchor = char '$' $> EndAnchor

choose :: Parser Choose
choose = chooseSpecialChar <|> chooseCharRange <|> chooseOneChar
 where
  chooseSpecialChar = ChooseSpecialChar <$> specialChar
  chooseCharRange   = ChooseCharRange <$> anyChar <* char '-' <*> anyChar
  chooseOneChar     = ChooseOneChar <$> anyChar

specialChar :: Parser SpecialChar
specialChar =
  whitespace
    <|> number
    <|> alphaNumeric
    <|> slash
    <|> tab
    <|> verticalTab
    <|> cr
    <|> lf
    <|> crlf
    <|> escape
    <|> backspace
    <|> formFeed
    <|> alert
    <|> nullEscape
    <|> octalEscape
    <|> hexadecimalEscape
    <|> escapedChar
 where
  whitespace        = constParse Whitespace "\\s"
  number            = constParse Number "\\d"
  alphaNumeric      = constParse Whitespace "\\w"
  slash             = constParse Slash "\\B"
  tab               = constParse Tab "\\t"
  verticalTab       = constParse VerticalTab "\\v"
  cr                = constParse CR "\\r"
  lf                = constParse LF "\\n"
  crlf              = constParse CRLF "\\R"
  escape            = constParse Escape "\\e"
  backspace         = constParse Backspace "\\b"
  formFeed          = constParse FormFeed "\\f"
  alert             = constParse Alert "\\a"
  nullEscape        = constParse NullEscape "\0"
  octalEscape       = OctalEscape <$> (constParse () "\\" *> error "not implemented")
  hexadecimalEscape = HexadecimalEscape . chr <$> (constParse () "\\" *> hexadecimal)
  escapedChar       = EscapedChar <$> (constParse () "\\" *> anyChar)


constParse :: a -> String -> Parser a
constParse a s = string (T.pack s) $> a

parseUntil :: Parser a -> Parser b -> Parser [a]
parseUntil parser til = (:) <$> parser <*> manyTill parser til

parseAllWithin :: String -> Parser a -> String -> Parser [a]
parseAllWithin start parser end = (string $ T.pack start) *> manyTill parser (string $ T.pack end)

parseWithin :: String -> Parser a -> String -> Parser a
parseWithin start parser end = (string $ T.pack start) *> parser <* (string $ T.pack end)


