module Pipes.Prelude

import Control.Monad.Trans
import Pipes.Core

%access public export

-- Helper functions to construct Sources more easily
-- * `stdinLn` lifts the standard output to a Source

stdinLn : Source String IO r
stdinLn = do
  l <- lift getLine
  yield l
  stdinLn

-- Helper functions to construct pipes more easily
-- * `mapping` lifts a function as a pipe transformation
-- * `filtering` lifts a predicate into a pipe filter
-- * `takingWhile` lifts a predicate into a pipe breaker
-- * `droppingWhile` lifts a predicate into a pipe delayed starter

mapping : (Monad m) => (a -> b) -> Pipe a b m r
mapping f = recur where
  recur = do
    a <- await
    yield (f a)
    recur

mappingM : (Monad m) => (a -> m b) -> Pipe a b m r
mappingM f = recur where
  recur = do
    a <- await
    b <- lift (f a)
    yield b
    recur

filtering : (Monad m) => (a -> Bool) -> Pipe a a m r
filtering p = recur where
  recur = do
    a <- await
    if p a
      then yield a *> recur
      else recur

takingWhile : (Monad m) => (a -> Bool) -> Pipe a a m ()
takingWhile p = recur where
  recur = do
    a <- await
    if p a
      then yield a *> recur
      else pure ()

droppingWhile : (Monad m) => (a -> Bool) -> Pipe a a m r
droppingWhile p = recur where
  recur = do
    a <- await
    if p a
      then recur
      else yield a *> idP

-- Helper functions to construct Sinks more easily
-- * `stdoutLn` lifts the standard output to a Sink

stdoutLn : Sink String IO r
stdoutLn = do
  l <- await
  lift (putStrLn l)
  stdoutLn

--
