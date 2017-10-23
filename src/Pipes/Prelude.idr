module Pipes.Prelude

import Control.Monad.Trans
import Pipes.Core

%access public export

-- Helper functions to construct Sources more easily
-- * `stdinLn` lifts the standard output to a Source
-- * `iterating` creates an infinite series of value f(f(f(f(...))))
-- * `unfolding` creates a possibly infinite series of value from a seed

stdinLn : String -> Source String IO r
stdinLn promptLine = recur where
  recur = do
    lift (putStr promptLine)
    lift getLine >>= yield
    recur

iterating : (Monad m) => (a -> a) -> a -> Source a m r
iterating f = recur where
  recur a = do
    yield a
    recur (f a)

unfolding : (Monad m) => (seed -> Maybe (a, seed)) -> seed -> Source a m ()
unfolding f = recur . f where
  recur Nothing = pure ()
  recur (Just (a, seed)) = yield a *> recur (f seed)

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

repeating : (Monad m) => Nat -> Pipe a a m r
repeating n = recur where
  recur = do
    a <- await
    sequence_ (replicate n (yield a))
    recur


-- Helper functions to construct Sinks more easily
-- * `stdoutLn` lifts the standard output to a Sink
-- * `discard` consumes all outputs and ignore them

stdoutLn : Sink String IO r
stdoutLn = do
  l <- await
  lift (putStrLn l)
  stdoutLn

discard : (Monad m) => Source a m r
discard = recur where
  recur = await *> recur

--
