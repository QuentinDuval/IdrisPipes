module Pipes.Core

import public Control.Monad.Identity
import public Control.Monad.Trans



||| Main data type for Pipes:
||| * `a` is the type flowing in from the upstream
||| * `b` is the type flowing out to downstream
||| * `m` is the Monad the pipe runs in
||| * `r` is the returned value at pipe termination

export
data PipeM : (a, b, r1 : Type) -> (m : Type -> Type) -> (r2 : Type) -> Type where
  Pure    : r2 -> PipeM a b r1 m r2                                 -- Lift a value into the pipe
  Action  : (m (Inf (PipeM a b r1 m r2))) -> PipeM a b r1 m r2      -- Interleave an effect
  Yield   : Inf (PipeM a b r1 m r2) -> b -> PipeM a b r1 m r2       -- Yield a value and next status
  Await   : (Either r1 a -> PipeM a b r1 m r2) -> PipeM a b r1 m r2 -- Yield a continuation (expecting a value)

||| `yield` sends a value downstream
export
yield : b -> PipeM a b r1 m ()
yield = Yield (Pure ())

||| `await` waits from an upstream value
export
await : PipeM a b r1 m (Maybe a)
await = Await $ \v =>
  case v of Left _ => Pure (the (Maybe a) Nothing)
            Right x => Pure (Just x)

||| `awaitOr` waits from an upstream value, or result of upstream pipe
export
awaitOr : PipeM a b r1 m (Either r1 a)
awaitOr = Await Pure

||| A `Source` cannot `await` any input
public export
Source : (b: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Source b m r = PipeM Void b Void m r

||| A `Pipe` can `await` and `yield`
public export
Pipe : {r: Type} -> (a: Type) -> (m: Type -> Type) -> (b: Type) -> Type
Pipe {r} a m b = PipeM a b r m r

||| A `Sink` cannot `yield` anything
public export
SinkM : (a: Type) -> (m: Type -> Type) -> (r1, r2: Type) -> Type
SinkM a m r1 r2 = PipeM a Void r1 m r2

public export
Sink : {r1: Type} -> (a: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Sink {r1} a m r = SinkM a m r1 r

||| An `Effect` cannot `await` or `yield` (pure effect)
public export
Effect : (m: Type -> Type) -> (r: Type) -> Type
Effect m r = PipeM Void Void Void m r

||| Functor implementation: (Recursively replace `r` with `f r`)
export
implementation (Monad m) => Functor (PipeM a b r1 m) where
  map f = assert_total recur where
    recur (Pure r) = Pure (f r)
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (recur . cont)

||| Applicative implementation (Recursively replace `r` with `map r pa`)
export
implementation (Monad m) => Applicative (PipeM a b r1 m) where
  pure = Pure
  pf <*> pa = assert_total (recur pf) where
    recur (Pure f) = map f pa
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (recur . cont)

||| Monad implementation (Recursively replace `r` with `f r`)
export
implementation (Monad m) => Monad (PipeM a b r1 m) where
  m >>= f = assert_total (recur m) where
    recur (Pure r) = f r
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (recur . cont)

||| Monad Transformer implementation
||| * Wrap the monadic action in a `Action` constructor
||| * Wrap the monadic return in a `Pure` constructor
export
implementation MonadTrans (PipeM a b r1) where
  lift m = assert_total $ Action (m >>= \x => pure (Pure x))

infixl 9 .|
infixr 9 >~

mutual

  ||| Assembling pipes: Pull based behavior
  ||| - Run the actions of the right pipe until it reaches `await`
  ||| - The run the actions of the left pipe until it reached `yield`
  export
  (.|) : (Monad m) => PipeM a b r1 m r2 -> PipeM b c r2 m r3 -> PipeM a c r1 m r3
  (.|) up (Yield next c) = Yield (up .| next) c             -- Yielding downstream
  (.|) up (Action a) = lift a >>= \next => up .| next       -- Produce effect downstream
  (.|) up (Await cont) = up >~ cont                         -- Ask upstream for a value
  (.|) up (Pure r) = Pure r

  (>~) : (Monad m) => PipeM a b r1 m r2 -> (Either r2 b -> PipeM b c r2 m r3) -> PipeM a c r1 m r3
  (>~) (Await cont) down = Await (\a => cont a >~ down)     -- Awaiting upstream
  (>~) (Action a) down = lift a >>= \next => next >~ down   -- Produce effect upstream
  (>~) (Yield next b) down = next .| down (Right b)          -- Give control downstream
  (>~) (Pure r) down = Pure r .| down (Left r)               -- Termination, send Nothing to next


||| Running a Effect
||| * Execute the sequence of effects of the pipe
||| * Return the final value when no effects are remaining
export
runEffect : (Monad m) => Effect m r -> m r
runEffect (Pure r) = pure r                         -- Done executing the pipe, return the result
runEffect (Action a) = a >>= \p => runEffect p      -- Execute the action, run the next of the pipe
runEffect (Yield next b) = absurd b
runEffect (Await cont) = runEffect $ Await (either absurd absurd)

export
runPure : Effect Identity r -> r
runPure = runIdentity . runEffect

||| Consuming a Source: summarize a set of values into a single output value
export
foldM : (Monad m) => (a -> b -> m b) -> b -> Sink a m b
foldM f = recur where
  recur acc = do
    ma <- await
    case ma of
      Just x => lift (f x acc) >>= recur
      Nothing => pure acc

export
fold : (Monad m) => (a -> b -> b) -> b -> Sink a m b
fold f = foldM (\a, b => pure (f a b))

||| Helpers to build standard pipes
||| * `idP` creates the identity Pipe
||| * `each` lifts a foldable to a Source

export
idP : (Monad m) => Pipe a m a
idP = Await (either Pure (Yield idP))

export
each : (Monad m, Foldable f) => f a -> Source a m ()
each xs = foldr (\x, p => yield x *> p) (pure ()) xs

export
awaitForever : (Monad m) => (i -> PipeM i o r m ()) -> PipeM i o r m r
awaitForever f = recur where
  recur = do
    x <- awaitOr
    case x of
      Left r => pure r
      Right x => do f x; recur

--
