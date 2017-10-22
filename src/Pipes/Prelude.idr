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

concatMapping : (Monad m, Foldable f) => Pipe (f a) a m r
concatMapping = do
  xs <- await
  foldr (\x, p => yield x *> p) (pure ()) xs
  concatMapping

filtering : (Monad m) => (a -> Bool) -> Pipe a a m r
filtering p = recur where
  recur = do
    a <- await
    if p a
      then yield a *> recur
      else recur

taking : (Monad m) => Nat -> Pipe a a m ()
taking Z = pure ()
taking (S n) = await >>= yield *> taking n

dropping : (Monad m) => Nat -> Pipe a a m r
dropping Z = idP
dropping (S n) = await *> dropping n

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

deduplicating : (Eq a, Monad m) => Pipe a a m r
deduplicating = await >>= \a => yield a *> recur a where
  recur previous = do
    a <- await
    if a == previous
      then recur previous
      else yield a *> recur a


-- Helper functions to construct Sinks more easily
-- * `stdoutLn` lifts the standard output to a Sink

stdoutLn : Sink String IO r
stdoutLn = do
  l <- await
  lift (putStrLn l)
  stdoutLn

--
