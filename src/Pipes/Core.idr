module Pipes.Core

import public Control.Monad.Identity
import public Control.Monad.Trans


||| Main data type for Pipes:
||| * `a` is the type flowing in from the upstream
||| * `b` is the type flowing out to downstream
||| * `m` is the Monad the pipe runs in
||| * `r2` is the returned value of the pipe
||| * `r1` is the returned value of the previous pipe

export
data PipeM : (a, b, r1 : Type) -> (m : Type -> Type) -> (r2 : Type) -> Type where
  Pure    : r2 -> PipeM a b r1 m r2                                       -- Lift a value into the pipe
  Action  : m (Inf (PipeM a b r1 m r2)) -> PipeM a b r1 m r2              -- Interleave an effect
  Yield   : Inf (PipeM a b r1 m r2) -> b -> PipeM a b r1 m r2             -- Yield a value and next status
  Await   : (Either r1 a -> Inf (PipeM a b r1 m r2)) -> PipeM a b r1 m r2 -- Yield a continuation (expecting a value)

||| `yield` sends a value downstream

export
yield : b -> PipeM a b r' m ()
yield = Yield (Pure ())

||| `await` waits from an upstream value

export
await : PipeM a b r' m (Maybe a)
await = Await $ \v =>
  case v of Right x => Pure (Just x)
            _ => Pure (the (Maybe a) Nothing)

||| `awaitOr` waits from an upstream value, or result of upstream pipe

export
awaitOr : PipeM a b r1 m (Either r1 a)
awaitOr = Await $ \v => Pure v

||| A `Source` cannot `await` any input

public export
Source : (m: Type -> Type) -> (b: Type) -> Type
Source m b = PipeM Void b Void m ()

||| A `Source` that can return a result of type `r`

public export
SourceM : (b: Type) -> (m: Type -> Type) -> (r: Type) -> Type
SourceM b m r = PipeM Void b Void m r

||| A `Pipe` can `await` and `yield`

public export
Pipe : {r: Type} -> (a: Type) -> (m: Type -> Type) -> (b: Type) -> Type
Pipe {r} a m b = PipeM a b r m r

||| A `Sink` cannot `yield` anything

public export
Sink : {r1: Type} -> (a: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Sink {r1} a m r = PipeM a Void r1 m r

||| A `Sink` with access to the previous pipe return value

public export
SinkM : (a: Type) -> (m: Type -> Type) -> (r1, r2: Type) -> Type
SinkM a m r1 r2 = PipeM a Void r1 m r2

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
    recur (Await cont) = Await (\x => recur (cont x))

||| Applicative implementation (Recursively replace `r` with `map r pa`)

export
implementation (Monad m) => Applicative (PipeM a b r1 m) where
  pure = Pure
  pf <*> pa = assert_total (recur pf) where
    recur (Pure f) = map f pa
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (\x => recur (cont x))

||| Monad implementation (Recursively replace `r` with `f r`)

export
implementation (Monad m) => Monad (PipeM a b r1 m) where
  m >>= f = assert_total (recur m) where
    recur (Pure r) = f r
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (\x => recur (cont x))

||| Monad Transformer implementation
||| * Wrap the monadic action in a `Action` constructor
||| * Wrap the monadic return in a `Pure` constructor

export
implementation MonadTrans (PipeM a b r1) where
  lift m = assert_total $ Action (m >>= \x => pure (Pure x))

infixr 9 .|

||| Assembling pipes: Pull based behavior
||| * Run the actions of the right pipe until it reaches `await`
||| * The run the actions of the left pipe until it reached `yield`

export
(.|) : (Monad m) => PipeM a b r1 m r2 -> PipeM b c r2 m r3 -> PipeM a c r1 m r3
(.|) = pull where
  mutual

    pull : (Monad m) => PipeM a b r1 m r2 -> PipeM b c r2 m r3 -> PipeM a c r1 m r3
    pull up (Yield next c) = Yield (up `pull` next) c         -- Yielding downstream
    pull up (Action a) = lift a >>= \next => up `pull` next   -- Produce effect downstream
    pull up (Await cont) = up `push` \x => cont x             -- Ask upstream for a value
    pull up (Pure r) = Pure r

    push : (Monad m) => PipeM a b r1 m r2 -> (Either r2 b -> PipeM b c r2 m r3) -> PipeM a c r1 m r3
    push (Await cont) down = Await (\a => cont a `push` down)   -- Awaiting upstream
    push (Action a) down = lift a >>= \next => next `push` down -- Produce effect upstream
    push (Yield next b) down = next `pull` down (Right b)       -- Give control downstream
    push (Pure r) down = Pure r `pull` down (Left r)            -- Termination, send Nothing to next


||| Run an Effect and collect the output
||| * Execute the sequence of effects of the pipe
||| * Return the final value when no effects are remaining

export
runPipe : (Monad m) => Effect m r -> m r
runPipe (Pure r) = pure r                         -- Done executing the pipe, return the result
runPipe (Action a) = a >>= \p => runPipe p        -- Execute the action, run the next of the pipe
runPipe (Yield next b) = absurd b
runPipe (Await cont) = runPipe $ Await (either absurd absurd)

||| Run an Effect and discard the output

export
runEffect : (Monad m) => Effect m r -> m ()
runEffect p = runPipe p *> pure ()

||| Run an pure Effect in the Identity Monad

export
runPure : Effect Identity r -> r
runPure = runIdentity . runPipe

||| `foldM` consumes a stream
||| It creates a Sink to fold a set of values into a single output value

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

||| `idP` is the identity Pipe

export
idP : (Monad m) => Pipe a m a
idP = awaitOr >>= either Pure (Yield idP)

||| The function `each` lifts a foldable to a Source
||| * The actual type is in fact more general
||| * It allows you to use it to build pipes

export
each : (Monad m, Foldable f) => f a -> PipeM a' a r m ()
each xs = foldr (\x, p => yield x *> p) (pure ()) xs

||| Use `awaitOne` to automatically take care of forwarding the return value
||| of the previous pipe to the next pipe

export
awaitOne : (Monad m) => (i -> PipeM i o r m r) -> PipeM i o r m r
awaitOne f = awaitOr >>= either Pure f

||| Use `awaitForever` to build stateless pipes

export
awaitForever : (Monad m) => (i -> PipeM i o r m r') -> PipeM i o r m r
awaitForever f = recur where
  recur = awaitOr >>= either Pure (\x => do f x; recur)

--
