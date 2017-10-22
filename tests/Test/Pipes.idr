module Test.Pipes

import Pipes
import Test.Utils


--------------------------------------------------------------------------------
-- Pure pipelines
--------------------------------------------------------------------------------

even : Int -> Bool
even n = mod n 2 == 0

should_fold_ints : Test
should_fold_ints = do
  let ints = each [1..10] .| filtering even .| mapping (*2)
  assertEq 60 (runIdentity $ fold (+) 0 ints)
  assertEq 122880 (runIdentity $ fold (*) 1 ints)

--------------------------------------------------------------------------------

isVowel : Char -> Bool
isVowel c = c `elem` (unpack "aeiou")

should_fold_strings : Test
should_fold_strings = do
  let strs = each ['a'..'z'] .| filtering isVowel
  assertEq "uoiea" (pack (runIdentity $ fold (::) [] strs))


--------------------------------------------------------------------------------
-- Effectful pipes
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- All tests
--------------------------------------------------------------------------------

export
run_tests : IO ()
run_tests = runTestSuite
    [ should_fold_ints
    , should_fold_strings
    ]

--
