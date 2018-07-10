{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FunCheck.Data.Regex
  ( provideMatch
  , tryProvideMatch
  )
where

import           Data.Foldable
import           Data.Monoid
import           FunCheck.Data.Alg
import           Text.Parsec.Error
import           Text.Regex.TDFA.Pattern
import           Text.Regex.TDFA.ReadRegex
import qualified Data.CharSet                  as CS

tryProvideMatch :: Applicative f
                => RegularGrammarAlg f
                -> CS.CharSet
                -> String
                -> Either ParseError (f Char)
tryProvideMatch t cs s = provideMatch t cs . fst <$> parseRegex s

provideMatch :: forall f . Applicative f => RegularGrammarAlg f -> CS.CharSet -> Pattern -> f Char
provideMatch t chars = matchAll
 where
  lit'     = lit t
  oneOf'   = oneOf t
  repeatN' = repeatN t
  plus'    = plus t
  star'    = star t

  chainAll' :: [f Char] -> f Char
  chainAll' (fa : fas) = chainAll t fa fas
  chainAll' []         = lit' '\0'

  charList = CS.toList chars

  matchAll :: Pattern -> f Char
  matchAll PEmpty                = lit' '\0'
  matchAll (PGroup _ sub       ) = matchAll sub
  matchAll (POr     ps         ) = oneOf' (matchAll <$> ps)
  matchAll (PConcat ps         ) = chainAll' (matchAll <$> ps)
  matchAll (PQuest  p          ) = repeatN' (Just 0, Just 1) (matchAll p)
  matchAll (PPlus   p          ) = plus' (matchAll p)
  matchAll (PStar _ p          ) = star' (matchAll p)
  matchAll (PBound mx Nothing p) = repeatN' (Nothing, Just mx) (matchAll p)
  matchAll (PBound mn mx      p) = repeatN' (Just mn, mx) (matchAll p)
  matchAll (PCarat  _          ) = lit' '\0'
  matchAll (PDollar _          ) = lit' '\0'
  matchAll (PDot    _          ) = oneOf' (lit' <$> charList)
  matchAll (PAny    _ pSet     ) = oneOf' (lit' <$> reducePatternSet pSet)
  matchAll (PAnyNot _ pSet) = oneOf' (lit' <$> filter (`notElem` reducePatternSet pSet) charList)
  matchAll (PEscape _ c        ) = lit' c
  matchAll (PChar   _ c        ) = lit' c
  matchAll (PNonCapture p      ) = matchAll p
  matchAll (PNonEmpty   p      ) = matchAll p


  reducePatternSet :: PatternSet -> String
  reducePatternSet (PatternSet maybeChars maybeClasses maybeCollatingElements maybeEqvClass) =
    maybeFToList maybeChars
      <> (unSCC =<< maybeFToList maybeClasses)
      <> (unSCE =<< maybeFToList maybeCollatingElements)
      <> (unSEC =<< maybeFToList maybeEqvClass)

  maybeFToList :: Foldable h => Maybe (h a) -> [a]
  maybeFToList = maybe [] toList
