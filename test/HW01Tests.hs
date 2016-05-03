-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF1 "lastDigit" lastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , testF1 "dropLastDigit" dropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "toRevDigits" toRevDigits
             [(12340, [0,4,3,2,1]), (1, [1]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "doubleEveryOther" doubleEveryOther
             [([], []), ([1], [1]), ([1, 1], [1, 2])]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF1 "sumDigits" sumDigits
             [([], 0), ([10, 5, 18, 4] , 19)]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF1 "luhn" luhn
             [(5594589764218858, True), (1234567898765432 , False)]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ testF4 "hanoi" hanoi
             [(2, "a", "b", "c", [("a","c"), ("a","b"), ("c","b")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
