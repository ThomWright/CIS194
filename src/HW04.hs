{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List (intercalate)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P p1) (P p2) = remTrailing0 p1 == remTrailing0 p2
      where remTrailing0 = reverse . dropWhile (0 ==) . reverse

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = handleZero $ intercalate " + " $ reverse $ filter (not . null) $ zipWith toTerm p [(0::Int)..]
      where
        toTerm 0    _ = ""
        toTerm c    0 = show c
        toTerm 1    1 = "x"
        toTerm (-1) 1 = "-x"
        toTerm c    1 = show c ++ "x"
        toTerm 1    e = "x^" ++ show e
        toTerm (-1) e = "-x^" ++ show e
        toTerm c    e = show c ++ "x^" ++ show e

        handleZero "" = "0"
        handleZero s  = s

-- Exercise 4 -----------------------------------------

-- http://stackoverflow.com/a/21349554
zipWithDefault :: a -> b -> (a->b->c) -> [a] -> [b] -> [c]
zipWithDefault da db f la lb = let len = max (length la) (length lb)
                                   la' = la ++ repeat da
                                   lb' = lb ++ repeat db
                               in take len $ zipWith f la' lb'

zipWith0 :: (Num a, Num b) => (a->b->c) -> [a] -> [b] -> [c]
zipWith0 = zipWithDefault 0 0

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = P $ zipWith0 (+) p1 p2

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) = sum $ prefixZeros $ multCoeffs p1 p2

multCoeffs :: Num a => [a] -> [a] -> [Poly a]
multCoeffs [] _      = []
multCoeffs (c:cs) a2 = (P $ map (c *) a2) : multCoeffs cs a2

prefixZeros :: Num a => [Poly a] -> [Poly a]
prefixZeros = zipWith prefix [0..]
  where
    prefix i (P poly) = P $ replicate i 0 ++ poly

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p)  = P $ map negate p
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P poly) n = sum $ zipWith applyTerm [(0::Int)..] poly
  where
    applyTerm e c = c*(n^e)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 f = deriv f
    nderiv n f = deriv $ nderiv (n-1) f

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P poly) = P $ decExp $ zipWith calcCoeff [(0::Int)..] poly
      where
        calcCoeff e c = fromIntegral e * c

        decExp []     = []
        decExp [_]    = []
        decExp (_:cs) = cs
