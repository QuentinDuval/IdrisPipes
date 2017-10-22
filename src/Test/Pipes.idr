module Test.Pipes

import Pipes
import Test.Utils

even : Int -> Bool
even n = mod n 2 == 0

should_fold : Test
should_fold = do
  r <- consume (+) 0 (each [1..10] .| filtering even .| mapping (*2))
  assertEq 60 r

export
run_tests : IO ()
run_tests = runTestSuite
    [ should_fold
    ]

--
