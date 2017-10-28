module Pipes.Prelude

import Control.Monad.Trans
import Pipes.Core

%access public export

-- Helper functions to construct Sources more easily
-- * `stdinLn` lifts the standard output to a Source
-- * `iterating` creates an infinite series of value f(f(f(f(...))))
-- * `unfolding` creates a possibly infinite series of value from a seed

stdinLn : String -> Source IO String
stdinLn promptLine = recur where
  recur = do
    lift (putStr promptLine)
    lift getLine >>= yield
    recur

iterating : (Monad m) => (a -> a) -> a -> Source m a
iterating f = recur where
  recur a = do
    yield a
    recur (f a)

unfolding : (Monad m) => (seed -> Maybe (a, seed)) -> seed -> Source m a
unfolding f = recur . f where
  recur Nothing = pure ()
  recur (Just (a, seed)) = do
    yield a
    recur (f seed)

-- Helper functions to construct pipes more easily
-- * `mapping` lifts a function as a pipe transformation
-- * `filtering` lifts a predicate into a pipe filter
-- * `takingWhile` lifts a predicate into a pipe breaker
-- * `droppingWhile` lifts a predicate into a pipe delayed starter

mapping : (Monad m) => (a -> b) -> Pipe a m b
mapping f = awaitForever (yield . f)

mappingM : (Monad m) => (a -> m b) -> Pipe a m b
mappingM f = awaitForever $ \x => lift (f x) >>= yield

concatting : (Monad m, Foldable f) => Pipe (f a) m a
concatting = awaitForever $ foldr (\x, p => yield x *> p) (pure ())

concatMapping : (Monad m, Foldable f) => (a -> f b) -> Pipe a m b
concatMapping f = mapping f .| concatting

filtering : (Monad m) => (a -> Bool) -> Pipe a m a
filtering p = awaitForever $ \x => if p x then yield x else pure ()

taking : (Monad m) => Nat -> Pipe a m a
taking Z = pure ()
taking (S n) = await >>= maybe (pure ()) (\x => yield x *> taking n)

dropping : (Monad m) => Nat -> Pipe a m a
dropping Z = idP
dropping (S n) = do
  mx <- await
  case mx of
    Just x => dropping n
    Nothing => pure ()

takingWhile : (Monad m) => (a -> Bool) -> Pipe a m a
takingWhile p = recur where
  recur = do
    mx <- await
    case mx of
      Just x => if p x then yield x *> recur else pure ()
      Nothing => pure ()

droppingWhile : (Monad m) => (a -> Bool) -> Pipe a m a
droppingWhile p = recur where
  recur = do
    mx <- await
    case mx of
      Just x => if p x then recur else yield x *> idP
      Nothing => pure ()

deduplicating : (Eq a, Monad m) => Pipe a m a
deduplicating = recur (the (a -> Bool) (const True)) where
  recur isDifferent = do
    mx <- await
    case mx of
      Just x => do
        when (isDifferent x) (yield x)
        recur (/= x)
      Nothing => pure ()

repeating : (Monad m) => Nat -> Pipe a m a
repeating n = awaitForever $ \x => sequence_ (replicate n (yield x))

groupingBy : (Monad m) => (a -> a -> Bool) -> Pipe a m (List a)
groupingBy sameGroup = recur (the (List a) []) where
  recur xs = do
    mx <- await
    case mx of
      Nothing => when (length xs > 0) (yield (reverse xs))
      Just y => do
        case xs of
          [] => recur [y]
          (x::_) =>
            if sameGroup x y
                then recur (y::xs)
                else do
                  yield (reverse xs)
                  recur [y]

grouping : (Monad m, Eq a) => Pipe a m (List a)
grouping = groupingBy (==)

tracing : (Monad m) => (a -> m ()) -> Pipe a m a
tracing trace = mappingM (\x => trace x *> pure x)


-- Helper functions to construct Sinks more easily
-- * `stdoutLn` lifts the standard output to a Sink
-- * `discard` consumes all outputs and ignore them

stdoutLn : Sink String IO ()
stdoutLn = awaitForever $ \x => lift (putStrLn x)

discard : (Monad m) => Sink a m ()
discard = awaitForever $ \_ => pure ()

summing : (Monad m, Num a) => Sink a m a
summing = fold (+) 0

multiplying : (Monad m, Num a) => Sink a m a
multiplying = fold (*) 1

consuming : (Monad m) => Sink a m (List a)
consuming = recur (the (List a -> List a) id) where
  recur diffList = do
    mx <- await
    case mx of
      Just x => recur (diffList . \l => x :: l)
      Nothing => pure (diffList [])

--
