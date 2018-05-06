module FunCheck.Data.Regex(Regex(..), Choose(..), SpecialChar(..)) where

import Data.Foldable
import Data.Map.Lazy
import qualified FunCheck.Data.Template as T
import System.Random
import Yaya.Control
import Yaya

data Regex
  = Lit String
  | StartAnchor Regex
  | EndAnchor Regex
  | OneOf [Choose]
  | NotOneOf [Choose]
  | CaptureGroup Regex
  | Repeat Int (Maybe Int) Bool Regex
  | Special SpecialChar
  | Or [Regex]
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

-- regexToTemplate :: (Recursive f) => Regex -> f (T.Template String)




-- regexToTemplateCoalg :: Regex -> T.Template String Regex
-- regexToTemplateCoalg (Lit s) = T.Lit s


instance Monoid Regex where
  mempty = And []

  mappend (And l) (And r) = And (l ++ r)
  mappend (And l) r       = And (l ++ [r])
  mappend l       (And r) = And (l : r)
  mappend (Or l) (Or r) = Or (l ++ r)
  mappend l r             = And (l:[r])
