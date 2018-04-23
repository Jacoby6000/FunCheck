module FunCheck.Data.Parse(parse) where

import FunCheck.Data.Model
import Control.Monad.Error

parse :: MonadError f String => String -> f Pattern

