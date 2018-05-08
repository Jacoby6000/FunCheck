{-# LANGUAGE FlexibleContexts #-}

module FunCheck.Parse.Regex
  (specialChar
  , choose
  , regexTerm
  )
where

import           Text.Parsec             hiding ( oneOf )
import           Data.Functor
import FunCheck.Data.Regex
import Text.ParserCombinators.Parsec.Expr

regex :: (Stream s m Char) => ParsecT s u m Regex
regex = error "foo"
 where
  term = buildExpressionParser ops regexTerm where

  greedyStar = Repeat True 0 Nothing <$ char '*'
  star       = Repeat False 0 Nothing <$ string "*?"
  greedyPlus = Repeat True 1 Nothing <$ char '+'
  plus       = Repeat False 1 Nothing <$ string "+?"

  _greedyRepeatRange :: (Stream s m Char) => ParsecT s u m (Int -> Int -> a)
  _greedyRepeatRange =
    (char '{' *> (digitToInt <$> many1 digit))
    <*> (optionMaybe $ char ',' *> many1 digit)
    <* char '}'

  greedyRepeatRange = RepeatRange True <$> _greedyRepeatRange
  repeatRange = RepeatRange False <$> (_greedyRepeatRange <* char '?')

  ops = [ [ postfix $ greedyStar
					, postfix $ star
          , postfix $ greedyPlus
          , postfix $ plus
          , postfix $ greedyRepeatRange
          , postfix $ repeatRange
          ]
        , [ Infix (Concat <$> regexTerm ++ regexTerm) AssocRight
          ]
        , [ binary "|" Or AssocRight
          ]
        , [ prefix "^" StartAnchor
          , postfix "$" EndAnchor
          ]
        ]


  binary  name fun assoc = Infix (do{ string name; return fun }) assoc
  prefix  name fun       = Prefix (do{ string name; return fun })
  postfix name fun       = Postfix (do{ string name; return fun })

regexTerm :: (Stream s m Char) => ParsecT s u m RegexTerm
regexTerm = choice [try wildcard, try notOneOf, try oneOf, literal]
 where
  brackets = between (char '[') (char ']')
  oneOf = Set True <$> brackets (many1 choose)
  notOneOf = Set False <$> brackets (char '^' *> many1 choose)
  wildcard = char '.' $> Wildcard
  literal = Literal <$> regexLiteral

choose :: (Stream s m Char) => ParsecT s u m Choose
choose = choice [try chooseCharRange, chooseOneChar] <?> "Choose"
 where
  chooseCharRange   = ChooseCharRange <$> anyChar <* char '-' <*> anyChar <?> "Choose Char Range"
  chooseOneChar     = ChooseLiteral <$> regexLiteral <?> "Choose One Char"

regexLiteral :: (Stream s m Char) => ParsecT s u m RegexLiteral
regexLiteral = choice [try regexSpecialChar, try regexEscape, regexChar]
 where
  regexSpecialChar = RegexSpecialChar <$> specialChar
  regexEscape = RegexEscape <$> (char '\\' *> anyChar)
  regexChar = RegexChar <$> noneOf "\\[]()*?+"

specialChar :: (Stream s m Char) => ParsecT s u m SpecialChar
specialChar = choice
  [ try whitespace
  , try number
  , try alphaNumeric
  , try octalEscape
  , try hexadecimalEscape
  ] <?> "Special Char"
 where
  whitespace        = constParse Whitespace "\\s"
  number            = constParse Number "\\d"
  alphaNumeric      = constParse Whitespace "\\w"
  octalEscape       = OctalEscape <$> (string "\\" *> error "not implemented")
  hexadecimalEscape = HexadecimalEscape <$> (string "\\" *> hexDigit)

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
