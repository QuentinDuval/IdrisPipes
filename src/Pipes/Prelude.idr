module Pipes.Prelude

import Control.Monad.Trans
import Pipes.Core

%access public export

-- Helper functions to construct Sources more easily
-- * `stdinLn` lifts the standard output to a Source
-- * `iterating` creates an infinite series of value f(f(f(f(...))))
-- * `unfolding` creates a possibly infinite series of value from a seed

stdinLn : String -> Source String IO ()
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

mapping : (Monad m) => (a -> b) -> Pipe a b m ()
mapping f = awaitForever (yield . f)

mappingM : (Monad m) => (a -> m b) -> Pipe a b m ()
mappingM f = awaitForever $ \x => lift (f x) >>= yield

concatting : (Monad m, Foldable f) => Pipe (f a) a m ()
concatting = awaitForever $ foldr (\x, p => yield x *> p) (pure ())

concatMapping : (Monad m, Foldable f) => (a -> f b) -> Pipe a b m ()
concatMapping f = mapping f .| concatting

filtering : (Monad m) => (a -> Bool) -> Pipe a a m ()
filtering p = awaitForever $ \x => if p x then yield x else pure ()

taking : (Monad m) => Nat -> Pipe a a m ()
taking Z = pure ()
taking (S n) = await >>= maybe (pure ()) (\x => yield x *> taking n)

dropping : (Monad m) => Nat -> Pipe a a m ()
dropping Z = idP
dropping (S n) = do
  mx <- await
  case mx of
    Just x => dropping n
    Nothing => pure ()

takingWhile : (Monad m) => (a -> Bool) -> Pipe a a m ()
takingWhile p = recur where
  recur = do
    mx <- await
    case mx of
      Just x => if p x then yield x *> recur else pure ()
      Nothing => pure ()

droppingWhile : (Monad m) => (a -> Bool) -> Pipe a a m ()
droppingWhile p = recur where
  recur = do
    mx <- await
    case mx of
      Just x => if p x then recur else yield x *> idP
      Nothing => pure ()

deduplicating : (Eq a, Monad m) => Pipe a a m ()
deduplicating = recur (the (a -> Bool) (const False)) where
  recur isPrevious = do
    mx <- await
    case mx of
      Just x => do
        when (not (isPrevious x)) (yield x)
        recur (/= x)
      Nothing => pure ()

repeating : (Monad m) => Nat -> Pipe a a m ()
repeating n = awaitForever $ \x => sequence_ (replicate n (yield x))

tracing : (Monad m) => (a -> m ()) -> Pipe a a m ()
tracing trace = mappingM (\x => trace x *> pure x)


-- Helper functions to construct Sinks more easily
-- * `stdoutLn` lifts the standard output to a Sink
-- * `discard` consumes all outputs and ignore them

stdoutLn : Sink String IO ()
stdoutLn = awaitForever $ \x => lift (putStrLn x)

discard : (Monad m) => Sink a m ()
discard = awaitForever $ \_ => pure ()

--
