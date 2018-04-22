module FunCheck.Data.Model(Pattern(..), SymbolRef, patternCata) where

data Pattern
  = Lit String
  | IntRange Int Int
  | DecRange Double Double
  | Optional Pattern
  | Var SymbolRef
  | Repeat Pattern (Maybe Int) (Maybe Int)
  | And [Pattern]
  | Or [Pattern]

newtype SymbolRef = SymbolRef String

patternCata :: (String -> a) -> (Int -> Int -> a) -> (Double -> Double -> a) -> (a -> a) -> (SymbolRef -> a) -> (a -> Maybe Int -> Maybe Int -> a) -> ([a] -> a) -> ([a] -> a) -> Pattern -> a
patternCata litF _ _ _ _ _ _ _ (Lit          s) = litF s
patternCata _ intF _ _ _ _ _ _ (IntRange mn mx) = intF mn mx
patternCata _ _ decF _ _ _ _ _ (DecRange mn mx) = decF mn mx
patternCata f g h optF i j k l (Optional   pat) = optF $ patternCata f g h optF i j k l pat
patternCata _ _ _ _ symF _ _ _ (Var        ref) = symF ref
patternCata f g h i j repF k l (Repeat p mn mx) = repF (patternCata f g h i j repF k l p) mn mx
patternCata f g h i j k andF l (And       pats) = andF $ patternCata f g h i j k andF l <$> pats
patternCata f g h i j k l orFn (Or        pats) = orFn $ patternCata f g h i j k l orFn <$> pats


