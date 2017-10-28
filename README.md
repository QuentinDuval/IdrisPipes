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

The following code creates a `Source` of `Int`, keeping only the even number and multiplying them by 2. We can consume this `Source` by summing or multiplying them:

    let ints = each [1..10] .| filtering even .| mapping (*2)

    runPure $ ints .| fold (+) 0
    > 60

    runPure $ ints .| fold (*) 1
    > 122880

We can interleave effects inside the computation in the pipe, for instance:

    runEffect $ each [1..10] .| mappingM (\x => printLn x *> pure x) .| fold (+) 0
    > 1   -- Print the first number going through the pipe
    > ...
    > 10  -- Print the last number going through the pipe
    > 55  -- The result of the computation = 1 + .. + 10

The pipe itself is lazy and pull-based. The downstream consumption will drive it. If we do not consume entirely, only those effects that are needed to reach this stage of the computation will be triggered and made visible:

    runEffect $ each [1..10] .| mappingM (\x => printLn x *> pure x) .| takingWhile (<5) .| fold (+) 0
    > 1   -- Print the first number going through the pipe
    > ...
    > 5   -- Print the last number going through the pipe
    > 10  -- The result of the computation = 1 + .. + 4
