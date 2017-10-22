module Test.Tutorial

import Pipes

echo_example : IO ()
echo_example = runEffect $
  stdinLn                       -- Read the standard output
    .| takingWhile (/= "quit")  -- Stop upon encountering "quit"
    .| mapping ("> " ++)        -- Add the prompt to the string
    .| stdoutLn                 -- Echo the string
