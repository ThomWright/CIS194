
import Testing
import qualified HW01Tests
import qualified HW02Tests
import qualified HW03Tests

allTests :: [Test]
allTests = concat [ HW01Tests.allTests
                  , HW02Tests.allTests
                  , HW03Tests.allTests
                  ]

main :: IO ()
main = do
  putStrLn "\n"
  let failures = runTests allTests
  case failures of
    [] -> putStrLn "Success"
    _ -> print failures

-- main:: IO ()
-- main = do
--   putStrLn "\n"
--   print $ show $ HW03Tests.runProgram factorial 4
