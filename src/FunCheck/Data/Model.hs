module FunCheck.Data.Model(Pattern(..), Symbol, patternCata) where

data Pattern
  = Lit String
  | IntRange Int Int
  | DecRange Double Double
  | Optional Pattern
  | Repeat Pattern (Maybe Int) (Maybe Int)
  | And [Pattern]
  | Or [Pattern]
  | Var Symbol
  | Let Symbol Pattern

newtype Symbol = Symbol String

patternCata :: (String -> a)
               -> (Int -> Int -> a)
               -> (Double -> Double -> a)
               -> (a -> a)
               -> (Symbol -> a)
               -> (Symbol -> a -> a)
               -> (a -> Maybe Int -> Maybe Int -> a)
               -> ([a] -> a)
               -> ([a] -> a)
               -> Pattern
               -> a

patternCata litF _ _ _ _ _ _ _ _ (Lit          s) = litF s
patternCata _ intF _ _ _ _ _ _ _ (IntRange mn mx) = intF mn mx
patternCata _ _ decF _ _ _ _ _ _ (DecRange mn mx) = decF mn mx
patternCata f g h optF i j k l m (Optional   pat) = optF $ patternCata f g h optF i j k l m pat
patternCata _ _ _ _ varF _ _ _ _ (Var        ref) = varF ref
patternCata f g h i j letF k l m (Let    sym pat) = letF sym $ patternCata f g h i j letF k l m pat
patternCata f g h i j k repF l m (Repeat p mn mx) = repF (patternCata f g h i j k repF l m p) mn mx
patternCata f g h i j k l andF m (And       pats) = andF $ patternCata f g h i j k l andF m <$> pats
patternCata f g h i j k l m orFn (Or        pats) = orFn $ patternCata f g h i j k l m orFn <$> pats


