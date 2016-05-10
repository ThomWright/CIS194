
import Testing
import qualified HW01Tests
import qualified HW02Tests

import HW02

allTests :: [Test]
allTests = HW01Tests.allTests ++ HW02Tests.allTests

main :: IO ()
main = do
  putStrLn "\n"
  let failures = runTests allTests
  case failures of
    [] -> putStrLn "Success"
    _ -> print failures
