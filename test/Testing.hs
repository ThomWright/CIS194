{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
module Testing where

import Data.Maybe
import Control.Arrow

data Test    = forall a. Show a => Test String (a -> Bool) [a]
data Failure = forall a. Show a => Fail String [a]

instance Show Failure where
    show (Fail name inputs) = "Failed Test \"" ++ name
                       ++ "\" on inputs " ++ show inputs

runTest :: Test -> Maybe Failure
runTest (Test name f inputs) = case filter (not . f) inputs of
                          [] -> Nothing
                          fs -> Just $ Fail name fs

runTests :: [Test] -> [Failure]
runTests = mapMaybe runTest

-- Helpers

testF1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> Test
testF1 name f l = Test name (uncurry (==)) $ map (first f) l

testF2 :: (Show a, Show b, Show c, Eq c) =>
  String -> (a -> b -> c) -> [(a, b, c)] -> Test
testF2 name f l = Test name (uncurry (==)) $ map (\(v, w, x) -> (f v w, x)) l

testF4 :: (Show a, Show b, Show c, Show d, Show e, Eq e) =>
  String -> (a -> b -> c -> d -> e) -> [(a, b, c, d, e)] -> Test
testF4 name f l = Test name (uncurry (==)) $ map (\(v, w, x, y, z) -> (f v w x y, z)) l
