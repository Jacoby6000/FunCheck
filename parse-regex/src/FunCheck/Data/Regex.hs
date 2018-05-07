module FunCheck.Data.Regex
  ( Regex(..)
  , Choose(..)
  , SpecialChar(..)
  , RegexTerm(..)
  , RegexLiteral(..)
  )
where

data Regex
  = Term RegexTerm
  | Repeat Int (Maybe Int) Bool Regex
  | Or Regex Regex
  | Group Regex
  | Concat Regex Regex
  | StartAnchor Regex
  | EndAnchor Regex
  deriving(Show, Eq)

data RegexTerm
  = Set Bool [Choose]
  | Literal RegexLiteral
  | Wildcard
  deriving(Show, Eq)

data RegexLiteral
  = RegexChar Char
  | RegexSpecialChar SpecialChar
  | RegexEscape Char
  deriving(Show, Eq)

data Choose
 = ChooseLiteral RegexLiteral
 | ChooseCharRange Char Char
  deriving(Show, Eq)

data SpecialChar
 = Whitespace
 | Number
 | AlphaNumeric
 | OctalEscape Char
 | HexadecimalEscape Char
  deriving(Show, Eq)

-- regexToTemplate :: (Recursive f) => Regex -> f (T.Template String)




-- regexToTemplateCoalg :: Regex -> T.Template String Regex
-- regexToTemplateCoalg (Lit s) = T.Lit s
