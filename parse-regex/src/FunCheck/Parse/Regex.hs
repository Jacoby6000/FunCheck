module FunCheck.Parse.Regex(Regex(..), Choose(..), SpecialChar(..), specialChar, choose, regex) where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import Data.Char

import qualified Data.Attoparsec.Text as A
-- import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T

data Regex
  = Lit String
  | OneOf [Choose]
  | NotOneOf [Choose]
  | CapureGroup Regex
  | Special SpecialChar
  | Optional Regex
  | Or Regex Regex
  | And Regex Regex
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

regex :: Parser Regex
regex = allButLit <|> lit
 where
  notOneOf = NotOneOf <$> parseAllWithin "[^" choose "]"
  oneOf = OneOf <$> parseAllWithin "[" choose "]"
  special = Special <$> specialChar

  allButLit = notOneOf <|> oneOf <|> special
  lit = Lit <$> A.manyTill A.anyChar allButLit

choose :: Parser Choose
choose = chooseSpecialChar <|> chooseCharRange <|> chooseOneChar
 where
  chooseSpecialChar = ChooseSpecialChar <$> specialChar
  chooseCharRange   = ChooseCharRange <$> A.anyChar <* constParse () "-" <*> A.anyChar
  chooseOneChar     = ChooseOneChar <$> A.anyChar

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
  hexadecimalEscape = HexadecimalEscape <$> chr <$> (constParse () "\\" *> A.hexadecimal)
  escapedChar       = EscapedChar <$> (constParse () "\\" *> A.anyChar)


constParse :: a -> String -> Parser a
constParse a s = A.asciiCI (T.pack s) *> pure a

parseAllWithin :: String -> Parser a -> String -> Parser [a]
parseAllWithin start parser end = constParse () start *> A.manyTill parser (constParse () end)

-- parseWithin :: String -> Parser a -> String -> Parser a
-- parseWithin start parser end = constParse () start *> parser <* constParse () end

-- constsParse :: a -> NonEmpty String -> Parser a
-- constsParse a (h :| t) = foldr ((<|>) . constParse a) (constParse a h) t


