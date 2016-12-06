{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import HW07.Cards

import Control.Monad()
import Control.Monad.Random
import Data.Functor()
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random()

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM = fmap

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = do
  iv <- v !? i
  jv <- v !? j
  return (v // [(i, jv), (j, iv)])

unsafeSwapV :: Int -> Int -> Vector a -> Vector a
unsafeSwapV i j v = v // [(i, jv), (j, iv)]
  where
    iv = v ! i
    jv = v ! j


-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM = traverse -- alternatively: sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  i <- getRandomR (0, length v)
  return (v !? i)


-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec 0 = return V.empty
randomVec l = do
  x  <- getRandom
  xs <- randomVec (l-1)
  return (cons x xs)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR 0 _ = return V.empty
randomVecR l bounds = do
  x  <- getRandomR bounds
  xs <- randomVecR (l-1) bounds
  return (cons x xs)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = case length v of
  0 -> return V.empty
  1 -> return v
  _ -> do let i = length v - 1
          j <- getRandomR (0, i)

          let swapped = unsafeSwapV i j v
          shuffledInit <- shuffle $ V.init swapped

          return (V.snoc shuffledInit (V.last swapped))

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v pivotIndex =
  let pivotVal = v ! pivotIndex
  in (
    V.filter (< pivotVal) v,
    pivotVal,
    V.filter (> pivotVal) v
  )

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | null v = V.empty
  | otherwise = qsort l <> V.singleton p <> qsort r
    where
      (l, p, r) = partitionAt v 0

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | null v = return V.empty
  | otherwise = do
    p <- getRandomR (0, length v - 1)

    let (l, m, r) = partitionAt v p
    left <- qsortR l
    right <- qsortR r

    return (left <> V.singleton m <> right)

-- Exercise 9 -----------------------------------------

-- Selection
-- Select the element with rank i in an unsorted array.
-- For example, select 0 v selects the minimum element, select (n - 1) v selects the maximum element, and select (n ‘div‘ 2) v selects the median.
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | null v        = return Nothing
  | i < 0         = return Nothing
  | i >= length v = return Nothing
  | otherwise     = do
    p <- getRandomR (0, length v - 1)

    let (l, m, r) = partitionAt v p
    let len = length l
    case compare i len of
      LT -> select i l
      EQ -> return (Just m)
      GT -> select (i - len - 1) r

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card label suit | suit <- suits, label <- labels ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
  | null deck = Nothing
  | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 deck = Just ([], deck)
getCards n deck = do
  (card, rest) <- nextCard deck
  (cards, rest') <- getCards (n-1) rest
  return (card:cards, rest')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
