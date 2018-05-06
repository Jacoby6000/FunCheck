{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Parse.Regex
  (specialChar
  , choose
  , regex
  )
where

import           Text.Parsec             hiding ( oneOf )
import           Data.Functor
import FunCheck.Data.Regex

regex :: (Stream s m Char) => ParsecT s u m Regex
regex =  reduce <$> choice [try or, try captureGroup, try startAnchor, try endAnchor, try notOneOf, oneOf, special, lit] <?> "Regex"
 where
  reduce (And [a]) = a
  reduce (Or [a]) = a
  reduce a = a

  brackets    = between (char '[') (char ']')
  parens      = between (char '(') (char ')')
  validChar   = noneOf "()|^$[]"
  allButOr    = choice [try captureGroup, startAnchor, try endAnchor, try notOneOf, oneOf, special, lit]

  startAnchor = StartAnchor <$> (char '^' *> regex)
  endAnchor   = EndAnchor <$> regex <* char '$'
  oneOf       = OneOf <$> brackets (many1 choose) <?> "One Of"
  notOneOf    = NotOneOf <$> brackets (char '^' *> many1 choose) <?> "Not One Of"
  captureGroup = CaptureGroup <$> parens regex <?> "Capture Group"
  special     = Special <$> specialChar <?> "Special"
  or          = Or <$> sepBy1 allButOr (char '|') <?> "Or"
  lit         = Lit <$> many1 validChar <?> "Lit"


choose :: (Stream s m Char) => ParsecT s u m Choose
choose = choice [try chooseSpecialChar, try chooseCharRange, chooseOneChar] <?> "Choose"
 where
  notEnd = noneOf "]"

  chooseSpecialChar = ChooseSpecialChar <$> specialChar <?> "Choose Special Char"
  chooseCharRange   = ChooseCharRange <$> notEnd <* char '-' <*> notEnd <?> "Choose Char Range"
  chooseOneChar     = ChooseOneChar <$> notEnd <?> "Choose One Char"

specialChar :: (Stream s m Char) => ParsecT s u m SpecialChar
specialChar = choice
  [ try whitespace
  , try number
  , try alphaNumeric
  , try slash
  , try tab
  , try verticalTab
  , try cr
  , try lf
  , try crlf
  , try escape
  , try backspace
  , try formFeed
  , try alert
  , try nullEscape
  , try octalEscape
  , try hexadecimalEscape
  , escapedChar
  ] <?> "Special Char"
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

constParse :: (Stream s m Char) => a -> String -> ParsecT s u m a
constParse a s = string s $> a

parseUntil :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
parseUntil parser til = (:) <$> parser <*> manyTill parser til

parseAllWithin :: (Stream s m Char) => String -> ParsecT s u m a -> String -> ParsecT s u m [a]
parseAllWithin start parser end = startParse *> manyTill parser endParse
 where
  startParse = string start
  endParse   = string end

parseWithin :: (Stream s m Char) => String -> ParsecT s u m a -> String -> ParsecT s u m a
parseWithin start parser end = string start *> parser <* string end
