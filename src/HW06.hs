{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor()

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 100 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons e s) = e : streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons e s) = Cons (f e) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat e = Cons e (sRepeat e)

sIterate :: (a -> a) -> a -> Stream a
sIterate f seed = Cons seed (sIterate f (f seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons e es) s2 = Cons e (sInterleave s2 es)

sTake :: Int -> Stream a -> [a]
sTake 0 _           = []
sTake n (Cons e es) = e : sTake (n-1) es

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (1+) 0

ruler :: Stream Integer
ruler = ruler' 0
  where
      ruler' n = sInterleave (sRepeat n) (ruler' (n+1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons n (rand n)
  where
    n = (1103515245 * seed + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 221 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax []     = Nothing
minMax (n:ns) = Just (minMax' (n, n) ns)
  where
    minMax' :: (Int, Int) -> [Int] -> (Int, Int)
    minMax' current [] = current
    minMax' (cMin, cMax) (x:xs)
      | x < cMin  = minMax' (x, cMax) xs
      | x > cMax  = minMax' (cMin, x) xs
      | otherwise = minMax' (cMin, cMax) xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
