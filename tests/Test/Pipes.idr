module Test.Pipes

import Pipes
import Test.Utils


--------------------------------------------------------------------------------
-- Pure pipelines
--------------------------------------------------------------------------------

even : Int -> Bool
even n = mod n 2 == 0

isVowel : Char -> Bool
isVowel c = c `elem` (unpack "aeiou")

should_fold : Test
should_fold = do
  sum <- fold (+) 0 (each [1..10] .| filtering even .| mapping (*2))
  assertEq 60 sum
  str <- fold (::) [] (each ['a'..'z'] .| filtering isVowel)
  assertEq "uoiea" (pack str)


--------------------------------------------------------------------------------
-- Effectful pipes
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- All tests
--------------------------------------------------------------------------------

export
run_tests : IO ()
run_tests = runTestSuite
    [ should_fold
    ]

--
