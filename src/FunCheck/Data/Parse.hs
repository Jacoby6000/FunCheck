module FunCheck.Data.Parse(parse) where

import FunCheck.Data.Model

parse :: MonadError f String => String -> f Pattern

