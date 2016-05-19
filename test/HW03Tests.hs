-- CIS 194, Spring 2015
--
-- Test cases for HW 03

module HW03Tests where

import HW03
import Testing

runProgram :: Statement -> Int -> Int
runProgram prog i = run (extend empty "In" i) prog "Out"

testFactorial :: [Test]
testFactorial = [ testF1 "factorial test" (runProgram factorial)
                  [ (4, 24)
                  ]
                ]

testSquareRoot :: [Test]
testSquareRoot = [ testF1 "squareRoot test" (runProgram squareRoot)
                  [ (25, 5)
                  ]
                ]

testFibonacci :: [Test]
testFibonacci = [ testF1 "fibonacci test" (runProgram fibonacci)
                  [ (10, 89)
                  ]
                ]

allTests :: [Test]
allTests = concat [ testFactorial
                  , testSquareRoot
                  , testFibonacci
                  ]
