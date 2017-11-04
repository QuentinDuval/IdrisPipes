module Test.Utils

import System

public export
Test : Type
Test = IO Int

export
assertThat : Bool -> String -> Test
assertThat test errorMsg =
  if test
    then do putStrLn "Test Passed"; pure 0
    else do putStrLn ("Test Failed: " ++ errorMsg); pure 1

export
assertEq : (Eq a, Show a) => (expected : a) -> (given : a) -> Test
assertEq e g =
  assertThat (g == e) $
    "Expected == " ++ show e ++ ", Got: " ++ show g

export
runTests : List Test -> Test
runTests = foldl (\res, t => (+) <$> res <*> t) (pure 0)

export
runTestSuite : List Test -> IO ()
runTestSuite tests = do
  failedCount <- runTests tests
  if failedCount > 0
    then exitFailure
    else pure ()

--
