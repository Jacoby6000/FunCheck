{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module FunCheck.Data.Regex
  ( provideMatch,
    tryProvideMatch
  )
where


import           Data.Foldable
import           Data.Monoid
import           FunCheck.Data.Gen
import           FunCheck.Data.TemplateAlg

import           Control.Monad.State.Lazy
import           System.Random
import           Text.Regex.TDFA.Pattern
import           Text.Regex.TDFA.ReadRegex
import           Text.Parsec.Error


tryProvideMatch :: (RandomGen g, MonadState g f)
                => RegularDataTemplateAlg f
                -> String
                -> Either ParseError (f String)
tryProvideMatch t s = provideMatch t . fst <$> parseRegex s

provideMatch :: forall g f
              . (RandomGen g, MonadState g f)
             => RegularDataTemplateAlg f
             -> Pattern
             -> f String
provideMatch t = matchAll
 where

  lit'      = lit t
  chainAll' = chainAll t (lit' "")
  oneOf'    = oneOf t
  repeatN'  = repeatN t
  plus'     = plus t
  star'     = star t

  combineAs :: Foldable h => f (h String) -> f String
  combineAs = fmap fold

  matchAll :: Pattern -> f String
  matchAll PEmpty         = lit' ""
  matchAll (PGroup _ sub) = matchAll sub
  matchAll (POr     ps  ) = oneOf' (matchAll <$> ps)
  matchAll (PConcat ps  ) = chainAll' (matchAll <$> ps)
  matchAll (PQuest  p   ) = combineAs $ repeatN' (Just 0, Just 1) (matchAll p)
  matchAll (PPlus   p   ) = combineAs $ plus' (matchAll p)
  matchAll (PStar _ p   ) = combineAs $ star' (matchAll p)
  matchAll (PBound mx Nothing p) =
    combineAs $ repeatN' (Nothing, Just mx) (matchAll p)
  matchAll (PBound mn mx p) = combineAs $ repeatN' (Just mn, mx) (matchAll p)
  matchAll (PCarat  _     ) = lit' ""
  matchAll (PDollar _     ) = lit' ""
  matchAll (PDot    _     ) = stateList random
  matchAll (PAny _ pSet   ) = stateList $ randomPick (reducePatternSet pSet)
  matchAll (PAnyNot _ pSet) =
    stateList $ randomPick (filter (`notElem` reducePatternSet pSet) charset)
  matchAll (PEscape _ c  ) = lit' [c]
  matchAll (PChar   _ c  ) = lit' [c]
  matchAll (PNonCapture p) = matchAll p
  matchAll (PNonEmpty   p) = matchAll p

  -- TODO: This is awful.  Make this better.
  charset
    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()-=_+\\][{}|';:\"?><,./`~\t\r\n"

  stateList :: (g -> (Char, g)) -> f String
  stateList s = (: []) <$> state s

  reducePatternSet :: PatternSet -> String
  reducePatternSet (PatternSet maybeChars maybeClasses maybeCollatingElements maybeEqvClass)
    = maybeFToList maybeChars
      <> (unSCC =<< maybeFToList maybeClasses)
      <> (unSCE =<< maybeFToList maybeCollatingElements)
      <> (unSEC =<< maybeFToList maybeEqvClass)

  maybeFToList :: Foldable h => Maybe (h a) -> [a]
  maybeFToList = maybe [] toList
