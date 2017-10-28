module Test.Pipes

import Pipes
import Test.Utils

%access public export


--------------------------------------------------------------------------------
-- Pure pipelines
--------------------------------------------------------------------------------

even : Int -> Bool
even n = mod n 2 == 0

test_filtering_mapping : Test
test_filtering_mapping = do
  let ints = each [1..10] .| filtering even .| mapping (*2)
  assertEq 60 $ runPure (ints .| summing)
  assertEq 122880 $ runPure (ints .| multiplying)

test_iterating_take : Test
test_iterating_take = do
  assertEq 55 $ runPure $ iterating (+1) 1 .| taking 10 .| summing
  assertEq 55 $ runPure $ iterating (+1) 1 .| takingWhile (< 11) .| summing

test_unfolding_drop : Test
test_unfolding_drop = do
  let source = unfolding (\x => Just (x, x + 1)) (-9)
  assertEq 55 $ runPure $ source .| dropping 10 .| taking 10 .| summing
  assertEq 55 $ runPure $ source .| droppingWhile (< 1) .| taking 10 .| summing

test_replicating_deduplicating : Test
test_replicating_deduplicating = do
  assertEq 110 $ runPure $ each [1..10] .| repeating 2 .| summing
  assertEq 55 $ runPure $ each [1..10] .| repeating 2 .| deduplicating .| summing

test_concatMapping : Test
test_concatMapping = do
  assertEq 110 $ runPure $ each [1..10] .| concatMapping (\x => [x, x]) .| summing

test_grouping : Test
test_grouping = do
  assertEq [2, 4, 6, 8, 10] $ runPure $ each [1..5] .| repeating 2 .| grouping .| mapping sum .| consuming


--------------------------------------------------------------------------------
-- Effectful pipes
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- All tests
--------------------------------------------------------------------------------

run_tests : IO ()
run_tests = runTestSuite
    [ test_filtering_mapping
    , test_iterating_take
    , test_unfolding_drop
    , test_replicating_deduplicating
    , test_concatMapping
    , test_grouping
    ]

--
