module FunCheck.Tests (tests) where

import Distribution.TestSuite
import FunCheck.Tests.Regex

tests :: IO [Test]
tests = return [regexTests]
