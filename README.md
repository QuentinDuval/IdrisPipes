# IdrisPipes

[![Build Status](https://travis-ci.org/QuentinDuval/IdrisPipes.svg?branch=master)](https://travis-ci.org/QuentinDuval/IdrisPipes)

<br>

## Goal

The goal of this package is to provide Idris with a library for composable and effectful production, transformation and consumption of streams of data. IdrisPipes aims at providing the means to write:

* Effectful programs, with side effects such as IO
* Over a stream of data, potentially infinite
* Efficiently, by streaming data and controlling memory consumption
* In a composable way, allowing problem decomposition and reuse

IdrisPipes is inspired by the work done in the Haskell community and in particular the following packages:

* https://hackage.haskell.org/package/pipes
* https://hackage.haskell.org/package/conduit

<br>

## Examples

The following code will continuously read inputs from the standard output, and echoing them with a prompt, until it receives a "quit" command and stops:

    echo_example : IO ()
    echo_example = runEffect $
      stdinLn                       -- Read the standard output
        .| takingWhile (/= "quit")  -- Stop upon encountering "quit"
        .| mapping ("> " ++)        -- Add the prompt to the string
        .| stdoutLn                 -- Echo the string

The following code walks over the integers from 1 to 10, keeping only the even number and multiplying them by 2. We can consume this stream of data by summing or multiplying them:

    let ints = each [1..10] .| filtering even .| mapping (*2)

    runPure $ ints .| fold (+) 0
    > 60

    runPure $ ints .| fold (*) 1
    > 122880

We can also interleave effects inside the computations over a stream of data. For instance, the following code will print the numbers that goes through the pipes:

    runPipe $ each [1..10] .| tracing printLn .| fold (+) 0
    > 1   -- Print the first number going through the pipe
    > ...
    > 10  -- Print the last number going through the pipe
    > 55  -- The result of the computation = 1 + .. + 10

The pipe follows pull-based streaming model. The downstream consumption will drive it, asking for more values when needed. It might not consume the stream entirely, in which case only those effects that are needed to reach this stage of the computation will be triggered and made visible:

    runPipe $ each [1..10] .| tracing printLn .| takingWhile (< 5) .| fold (+) 0
    > 1   -- Print the first number going through the pipe
    > ...
    > 5   -- Print the last number going through the pipe (not taken in the sum)
    > 10  -- The result of the computation = 1 + .. + 4

<br>

## Installing the package

You can install IdrisPipes by using the following `make` command:

    make install

The Makefile also contains some more targets that you may find useful:

    make build -- Build the library
    make test  -- Run the test suite
    make tuto  -- Run a REPL on the tutorial

<br>

## Documentation & Resources

You can find some more information on this library on my blog:

* https://deque.blog/2017/11/02/idrispipes-a-library-for-composable-and-effectful-stream-processing-in-idris/

<br>

