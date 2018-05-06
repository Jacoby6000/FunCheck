{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Parse.Regex(Regex(..), Choose(..), SpecialChar(..), specialChar, choose, regex) where

import Control.Applicative
import Text.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Char
import Data.Char
import Data.Functor

import qualified Data.Text as T

data Regex
  = Lit Char
  | StartAnchor
  | EndAnchor
  | OneOf [Choose]
  | NotOneOf [Choose]
  | CaptureGroup Regex
  | Repeat Int (Maybe Int) Regex
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

regex :: (Stream s m Char) => ParsecT s u m Regex
regex = oneOf <|> notOneOf
 where
  brackets = between (char '[') (char ']')
  oneOf = OneOf <$> (brackets $ many1 choose)
  notOneOf = NotOneOf <$> (brackets $ (char '^' *> many1 choose))


choose ::(Stream s m Char) =>  ParsecT s u m Choose
choose = choice [chooseSpecialChar, chooseCharRange, chooseOneChar]
 where
  chooseSpecialChar = ChooseSpecialChar <$> specialChar
  chooseCharRange   = ChooseCharRange <$> anyChar <* char '-' <*> anyChar
  chooseOneChar     = ChooseOneChar <$> anyChar

specialChar ::(Stream s m Char) =>  ParsecT s u m SpecialChar
specialChar =
  choice [
    whitespace,
    number,
    alphaNumeric,
    slash,
    tab,
    verticalTab,
    cr,
    lf,
    crlf,
    escape,
    backspace,
    formFeed,
    alert,
    nullEscape,
    octalEscape,
    hexadecimalEscape,
    escapedChar
  ]
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
  octalEscape       = OctalEscape <$> (string "\\" *> error "not implemented")
  hexadecimalEscape = HexadecimalEscape <$> (string "\\" *> hexDigit)
  escapedChar       = EscapedChar <$> (string "\\" *> anyChar)


constParse ::(Stream s m Char) =>  a -> String -> ParsecT s u m a
constParse a s = string s $> a

parseUntil :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
parseUntil parser til = (:) <$> parser <*> manyTill parser til

parseAllWithin :: (Stream s m Char) => String -> ParsecT s u m a -> String -> ParsecT s u m [a]
parseAllWithin start parser end = startParse *> manyTill parser endParse
 where
  startParse = string $  start
  endParse   = string $  end

parseWithin :: (Stream s m Char) => String -> ParsecT s u m a -> String -> ParsecT s u m a
parseWithin start parser end = string start *> parser <* string end

instance Monoid Regex where
  mempty = And []

  mappend (And l) (And r) = And (l ++ r)
  mappend (And l) r       = And (l ++ [r])
  mappend l       (And r) = And (l : r)
  mappend l r             = And (l:[r])

