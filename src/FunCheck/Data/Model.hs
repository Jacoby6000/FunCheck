module FunCheck.Data.Model(Pattern(..), SymbolRef, patternCata) where

{-
data RequestTemplate
  = AnyString
  | PatternString Pattern
  | Const String
  | Object [(String, RequestTemplate)]
  | List [RequestTemplate]
  | Integer Int
  | Var RefName
-}

data Pattern
  = Lit String
  | OneOf [Pattern]
  | IntRange Int Int
  | DecRange Double Double
  | Optional Pattern
  | Var SymbolRef
  | Repeat Pattern (Maybe Int) (Maybe Int)

newtype SymbolRef = SymbolRef String

patternCata :: (String -> a) -> ([a] -> a) -> (Int -> Int -> a) -> (Double -> Double -> a) -> (a -> a) -> (SymbolRef -> a) -> (Pattern -> Maybe Int -> Maybe Int -> a) -> Pattern -> a
patternCata litF _ _ _ _ _ _ (Lit          s) = litF s
patternCata f patF g h i j k (OneOf     pats) = patF $ (patternCata f patF g h i j k) <$> pats
patternCata _ _ intF _ _ _ _ (IntRange mn mx) = intF mn mx
patternCata _ _ _ decF _ _ _ (DecRange mn mx) = decF mn mx
patternCata f g h i optF j k (Optional   pat) = optF $ patternCata f g h i optF j k pat
patternCata _ _ _ _ _ symF _ (Var        ref) = symF ref
patternCata _ _ _ _ _ _ repF (Repeat p mn mx) = repF p mn mx

