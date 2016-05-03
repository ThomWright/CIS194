
import Testing
import HW01Tests


main :: IO ()
main = do
  putStrLn "\n"
  let failures = runTests HW01Tests.allTests
  case failures of
    [] -> putStrLn "Success"
    _ -> print failures
