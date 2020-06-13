module FunCheck.Tests.Util where

import Data.Time.Clock.System
import Data.Foldable
import Distribution.TestSuite
import qualified Data.CharSet as CS
import qualified Data.Set as Set

time :: IO Int
time = fromIntegral . systemSeconds <$> getSystemTime

test :: [String] -> String -> IO Progress -> TestInstance
test ts n r =
  TestInstance {run = r, name = n, tags = ts, options = [], setOption = \_ _ -> Right (test ts n r)}

shouldEqual :: (Eq a, Show a) => a -> a -> Progress
shouldEqual l r =
  if l == r then Finished Pass else failure (show l ++ " did not equal expected " ++ show r)

shouldBeOneOf :: (Eq a, Show a, Show (f a), Foldable f) => a -> f a -> Progress
shouldBeOneOf a as =
  if a `elem` as then Finished Pass else failure $ show a ++ " was not one of " ++ show as

containsOnly :: (Eq a, Foldable f, Foldable g, Ord a, Show (f a), Show (g a))
             => f a
             -> g a
             -> Progress
containsOnly as allowedAs =
  if Set.isSubsetOf (Set.fromList (toList as)) (Set.fromList (toList allowedAs))
    then Finished Pass
    else failure $ show as ++ " was not a subset of " ++ show allowedAs

errOr :: Show e => (a -> Progress) -> Either e a -> Progress
errOr = either (err . show)

err :: String -> Progress
err = Finished . Error

failure :: String -> Progress
failure = Finished . Fail

randomTest :: String
randomTest = "random"
