# IdrisPipes

[![Build Status](https://travis-ci.org/QuentinDuval/IdrisPipes.svg?branch=master)](https://travis-ci.org/QuentinDuval/IdrisPipes)

## Motivation

IdrisPipes is a library for Idris to build effectful pipes, inspired from the Haskell pipes library: https://hackage.haskell.org/package/pipes.

## Examples

The following code will continuously read inputs from the standard output, and echoing them with a prompt, until it receives a "quit" command and stops:

    echo_example : IO ()
    echo_example = runEffect $
      stdinLn                       -- Read the standard output
        .| takingWhile (/= "quit")  -- Stop upon encountering "quit"
        .| mapping ("> " ++)        -- Add the prompt to the string
        .| stdoutLn                 -- Echo the string
