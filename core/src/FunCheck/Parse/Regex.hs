module FunCheck.Parse.Regex(TemplateTok(..), Choose(..), SpecialChar(..), specialChar) where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import Data.Char
import Data.List.NonEmpty

import qualified Data.Attoparsec.Text as A
-- import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T

data TemplateTok
  = LitTok String
  | OneOfTok [Choose]
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
  deriving(Show, Eq)

specialChar :: Parser SpecialChar
specialChar =
  whitespace <|>
  number <|>
  alphaNumeric <|>
  slash <|>
  tab <|>
  verticalTab <|>
  cr <|>
  lf <|>
  crlf <|>
  escape <|>
  backspace <|>
  formFeed <|>
  alert <|>
  nullEscape <|>
  octalEscape <|>
  hexadecimalEscape
  where
    whitespace = constParse Whitespace "\\s"
    number = constParse Number "\\d"
    alphaNumeric = constParse Whitespace "\\w"
    slash = constsParse Slash $ "\\B" :| ["\\\\"]
    tab = constParse Tab "\\t"
    verticalTab = constParse VerticalTab "\\v"
    cr = constParse CR "\\r"
    lf = constParse LF "\\n"
    crlf = constParse CRLF "\\R"
    escape = constParse Escape "\\e"
    backspace = constParse Backspace "\\b"
    formFeed = constParse  FormFeed "\\f"
    alert = constParse Alert "\\a"
    nullEscape = constParse NullEscape "\0"
    octalEscape = OctalEscape <$> (constParse () "\\" *> error "not implemented")
    hexadecimalEscape = HexadecimalEscape <$> chr <$> (constParse () "\\" *> A.hexadecimal)


constParse :: a -> String -> Parser a
constParse a s = A.asciiCI (T.pack s) *> pure a

constsParse :: a -> NonEmpty String -> Parser a
constsParse a (h :| t) = foldr ((<|>) . constParse a) (constParse a h) t

