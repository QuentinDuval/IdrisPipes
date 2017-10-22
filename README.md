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

    runIdentity $ fold (+) 0 ints
    > 60

    runIdentity $ fold (*) 1 ints
    > 122880
