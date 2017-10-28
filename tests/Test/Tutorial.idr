module Test.Tutorial

import Pipes


--------------------------------------------------------------------------------
-- A simple echo program
--------------------------------------------------------------------------------

echo_setup : IO ()
echo_setup = disableBuffering

echo_bad : IO ()
echo_bad = do
  putStr "in> "
  l <- getLine                  -- Read the standard input
  when (l /= "quit") $ do       -- Stop upon encountering "quit"
    putStrLn ("out> " ++ l)     -- Echo the string in standard output
    echo_bad

echo_good : IO ()
echo_good = runEffect $
  stdinLn "in> "                -- Read the standard input
    .| takingWhile (/= "quit")  -- Stop upon encountering "quit"
    .| mapping ("out> " ++)     -- Add the prompt to the string
    .| stdoutLn                 -- Echo the string in standard output


-- Slight modification to the program:
-- * Do not repeat the same sentence twice
-- * See how it gets bad on the loop side

echo_once_bad : IO ()
echo_once_bad = loop (const True) where
  loop : (String -> Bool) -> IO ()
  loop isDifferent = do
    putStr "in> "
    l <- getLine                  -- Read the standard input
    when (l /= "quit") $ do       -- Stop upon encountering "quit"
      when (isDifferent l) $      -- Remove consecutive repeating calls
        putStrLn ("out> " ++ l)   -- Echo the string in standard output
      loop (/= l)                 -- Loop with last read string

echo_once_good : IO ()
echo_once_good = runEffect $
  stdinLn "in> "                -- Read the standard input
    .| takingWhile (/= "quit")  -- Stop upon encountering "quit"
    .| deduplicating            -- Remove consecutive repeating calls
    .| mapping ("out> " ++)     -- Add the prompt to the string
    .| stdoutLn                 -- Echo the string in standard output


--------------------------------------------------------------------------------
-- Just exploiting laziness
--------------------------------------------------------------------------------

sum_with_traces : IO ()
sum_with_traces = do
  r <- runEffect $ each [1..10] .| tracing printLn .| fold (+) 0
  printLn r

sum_with_limit : IO ()
sum_with_limit = do
  r <- runEffect $ each [1..10] .| tracing printLn .| takingWhile (< 5) .| fold (+) 0
  printLn r


--------------------------------------------------------------------------------

export
run_tutotial : IO ()
run_tutotial = do
  -- echo_example
  sum_with_traces
  sum_with_limit
