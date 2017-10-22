module Test.Tutorial

import Pipes

--------------------------------------------------------------------------------

echo_example : IO ()
echo_example = runEffect $
  stdinLn                       -- Read the standard output
    .| takingWhile (/= "quit")  -- Stop upon encountering "quit"
    .| mapping ("> " ++)        -- Add the prompt to the string
    .| stdoutLn                 -- Echo the string

sum_with_traces : IO ()
sum_with_traces = do
  r <- fold (+) 0 $ each [1..10] .| mappingM (\x => printLn x *> pure x)
  printLn r

--------------------------------------------------------------------------------

export
run_tutotial : IO ()
run_tutotial = do
  -- echo_example
  sum_with_traces
