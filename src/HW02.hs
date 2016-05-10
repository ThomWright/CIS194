{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Control.Monad (replicateM)

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

count :: Eq a => a -> [a] -> Int
count t = length . filter (==t)

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = count True $ zipWith (==) c1 c2

-- Exercise 2 -----------------------------------------

countIn :: Eq a => [a] -> a -> Int
countIn = flip count

-- For each peg in xs, count how many times it occurs in ys
countColors :: Code -> [Int]
countColors code = map (countIn code) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code guess = sum $ zipWith min (countColors code) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove code guess = Move guess exact nonExact
  where
    exact    = exactMatches code guess
    nonExact = matches code guess - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code = move == getMove code guess
  where
    (Move guess _ _) = move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = replicateM n colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = doMove $ allCodes (length secret)
  where
    doMove :: [Code] -> [Move]
    doMove (g:gs)
      | g == secret = [move]
      | otherwise   = move : doMove (filterCodes move gs)
      where move = getMove secret g
    doMove _ = []

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
