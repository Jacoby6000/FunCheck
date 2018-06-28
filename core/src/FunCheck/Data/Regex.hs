{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module FunCheck.Data.Regex
  ( provideMatch
  , tryProvideMatch
  )
where


import           System.Random

import           Control.Monad.State.Lazy
import qualified Data.CharSet                  as CS
import           Data.Foldable
import           Data.Monoid
import           Text.Parsec.Error
import           Text.Regex.TDFA.Pattern
import           Text.Regex.TDFA.ReadRegex

import           FunCheck.Data.Gen
import           FunCheck.Data.TemplateAlg



tryProvideMatch :: (RandomGen g, MonadState g f)
                => RegularDataTemplateAlg f
                -> CS.CharSet
                -> String
                -> Either ParseError (f String)
tryProvideMatch t cs s = provideMatch t cs . fst <$> parseRegex s

provideMatch :: forall g f
              . (RandomGen g, MonadState g f)
             => RegularDataTemplateAlg f
             -> CS.CharSet
             -> Pattern
             -> f String
provideMatch t chars = matchAll
 where
  lit'     = lit t
  oneOf'   = oneOf t
  repeatN' = repeatN t
  plus'    = plus t
  star'    = star t

  charList = CS.toList chars

  combineAs :: Foldable h => f (h String) -> f String
  combineAs = fmap fold

  matchAll :: Pattern -> f String
  matchAll PEmpty         = lit' ""
  matchAll (PGroup _ sub) = matchAll sub
  matchAll (POr     ps  ) = oneOf' (matchAll <$> ps)
  matchAll (PConcat ps  ) = join <$> traverse matchAll ps
  matchAll (PQuest  p   ) = combineAs $ repeatN' (Just 0, Just 1) (matchAll p)
  matchAll (PPlus   p   ) = combineAs $ plus' (matchAll p)
  matchAll (PStar _ p   ) = combineAs $ star' (matchAll p)
  matchAll (PBound mx Nothing p) =
    combineAs $ repeatN' (Nothing, Just mx) (matchAll p)
  matchAll (PBound mn mx p) = combineAs $ repeatN' (Just mn, mx) (matchAll p)
  matchAll (PCarat  _     ) = lit' ""
  matchAll (PDollar _     ) = lit' ""
  matchAll (PDot    _     ) = stateList $ randomPick charList
  matchAll (PAny _ pSet   ) = stateList $ randomPick (reducePatternSet pSet)
  matchAll (PAnyNot _ pSet) =
    stateList $ randomPick (filter (`notElem` reducePatternSet pSet) charList)
  matchAll (PEscape _ c  ) = lit' [c]
  matchAll (PChar   _ c  ) = lit' [c]
  matchAll (PNonCapture p) = matchAll p
  matchAll (PNonEmpty   p) = matchAll p


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
