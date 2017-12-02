module Test.Pipes

import Control.Monad.Writer
import Pipes
import Test.Utils

%access export


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

test_repeating_deduplicating : Test
test_repeating_deduplicating = do
  assertEq 110 $ runPure $ each [1..10] .| repeating 2 .| summing
  assertEq 55 $ runPure $ each [1..10] .| repeating 2 .| deduplicating .| summing

test_concatMapping : Test
test_concatMapping = do
  assertEq 110 $ runPure $ each [1..10] .| concatMapping (\x => [x, x]) .| summing

test_grouping : Test
test_grouping = do
  assertEq [2, 4, 6, 8, 10] $
    runPure (each [1..5] .| repeating 2 .| grouping .| mapping sum .| consuming)

test_chunking : Test
test_chunking = do
  assertEq [[1, 2], [3, 4], [5]] $
    runPure (each [1..5] .| chunking 2 .| consuming)

test_splitting : Test
test_splitting = do
  assertEq [[1], [3], [5]] $
    runPure (each [1..5] .| splittingBy even .| consuming)

test_replicating_scanning : Test
test_replicating_scanning = do
  assertEq [0..10] $
    runPure (replicating 10 1 .| scanning (+) 0 .| consuming)


--------------------------------------------------------------------------------
-- Effectful pipes
--------------------------------------------------------------------------------

test_tracing : Test
test_tracing = do
  (a, w) <- runWriterT $ runPipe (each [1..10] .| tracing (tell . show) .| discard)
  assertEq "12345678910" w

rightOr : r -> Either l r -> r
rightOr r (Left _) = r
rightOr _ (Right r) = r

test_reading_file : Test
test_reading_file = do
  r <- runPipe (readFile "./Test/test.txt" .| mapping (rightOr "") .| consuming)
  assertEq "123\n45678\n9\n" (concat r)

--------------------------------------------------------------------------------
-- All tests
--------------------------------------------------------------------------------

run_tests : IO ()
run_tests = runTestSuite
    [ test_filtering_mapping
    , test_iterating_take
    , test_unfolding_drop
    , test_repeating_deduplicating
    , test_concatMapping
    , test_grouping
    , test_chunking
    , test_splitting
    , test_replicating_scanning
    , test_tracing
    , test_reading_file
    ]

--
