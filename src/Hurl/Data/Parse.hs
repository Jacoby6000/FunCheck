module Hurl.Data.Parse(parse) where

import Hurl.Data.Model

parse :: MonadError f String => String -> f Pattern

