module FunCheck.Data.Model(Pattern(..), SymbolRef, patternCata) where

data Pattern
  = Lit String
  | OneOf [Pattern]
  | IntRange Int Int
  | DecRange Double Double
  | Optional Pattern
  | Var SymbolRef
  | Repeat Pattern (Maybe Int) (Maybe Int)
  | And [Pattern]
  | Or [Pattern]

newtype SymbolRef = SymbolRef String

patternCata :: (String -> a) -> ([a] -> a) -> (Int -> Int -> a) -> (Double -> Double -> a) -> (a -> a) -> (SymbolRef -> a) -> (a -> Maybe Int -> Maybe Int -> a) -> ([a] -> a) -> ([a] -> a) -> Pattern -> a
patternCata litF _ _ _ _ _ _ _ _ (Lit          s) = litF s
patternCata f patF g h i j k l m (OneOf     pats) = patF $ patternCata f patF g h i j k l m <$> pats
patternCata _ _ intF _ _ _ _ _ _ (IntRange mn mx) = intF mn mx
patternCata _ _ _ decF _ _ _ _ _ (DecRange mn mx) = decF mn mx
patternCata f g h i optF j k l m (Optional   pat) = optF $ patternCata f g h i optF j k l m pat
patternCata _ _ _ _ _ symF _ _ _ (Var        ref) = symF ref
patternCata f g h i j k repF l m (Repeat p mn mx) = repF (patternCata f g h i j k repF l m p) mn mx
patternCata f g h i j k l andF m (And       pats) = andF $ patternCata f g h i j k l andF m <$> pats
patternCata f g h i j k l m orFn (Or        pats) = orFn $ patternCata f g h i j k l m orFn <$> pats


