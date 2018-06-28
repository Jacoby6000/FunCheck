{-# LANGUAGE TypeOperators, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module FunCheck.Data.Regex
  ( provideMatch
  , tryProvideMatch
  )
where


import           System.Random

import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.Monoid
import           Text.Parsec.Error
import           Text.Regex.TDFA.Pattern
import           Text.Regex.TDFA.ReadRegex
import qualified Data.CharSet                  as CS

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
  lit'      = lit t
  oneOf'    = oneOf t
  repeatN'  = repeatN t
  plus'     = plus t
  star'     = star t
  chainAll' = chainAll t (lit' "")

  charList = CS.toList chars
  litChar c = lit' [c]

  matchAll :: Pattern -> f String
  matchAll PEmpty         = lit' ""
  matchAll (PGroup _ sub) = matchAll sub
  matchAll (POr     ps  ) = oneOf' (matchAll <$> ps)
  matchAll (PConcat ps  ) = chainAll' (matchAll <$> ps)
  matchAll (PQuest  p   ) = join <$> repeatN' (Just 0, Just 1) (matchAll p)
  matchAll (PPlus   p   ) = join . toList <$> (plus' (matchAll p))
  matchAll (PStar _ p   ) = join <$> star' (matchAll p)
  matchAll (PBound mx Nothing p) = join <$> repeatN' (Nothing, Just mx) (matchAll p)
  matchAll (PBound mn mx p) = join <$> repeatN' (Just mn, mx) (matchAll p)
  matchAll (PCarat  _     ) = lit' ""
  matchAll (PDollar _     ) = lit' ""
  matchAll (PDot    _     ) = oneOf' (litChar <$> charList)
  matchAll (PAny _ pSet   ) = oneOf' (litChar <$> reducePatternSet pSet)
  matchAll (PAnyNot _ pSet) = oneOf' (litChar <$> filter (`notElem` reducePatternSet pSet) charList)
  matchAll (PEscape _ c  ) = lit' [c]
  matchAll (PChar   _ c  ) = lit' [c]
  matchAll (PNonCapture p) = matchAll p
  matchAll (PNonEmpty   p) = matchAll p


  reducePatternSet :: PatternSet -> String
  reducePatternSet (PatternSet maybeChars maybeClasses maybeCollatingElements maybeEqvClass)
    = maybeFToList maybeChars
      <> (unSCC =<< maybeFToList maybeClasses)
      <> (unSCE =<< maybeFToList maybeCollatingElements)
      <> (unSEC =<< maybeFToList maybeEqvClass)

  maybeFToList :: Foldable h => Maybe (h a) -> [a]
  maybeFToList = maybe [] toList
