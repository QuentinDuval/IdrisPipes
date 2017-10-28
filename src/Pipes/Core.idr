module Pipes.Core

import public Control.Monad.Identity
import public Control.Monad.Trans


{-
Main data type for Pipes:
* not really a `codata`, does not really need `Inf`
* but most function will need `assert_total`
-}
export
data PipeM : (a, b : Type) -> (m : Type -> Type) -> (r : Type) -> Type where
  Pure    : r -> PipeM a b m r                            -- Lift a value into the pipe
  Action  : (m (Inf (PipeM a b m r))) -> PipeM a b m r    -- Interleave an effect
  Yield   : Inf (PipeM a b m r) -> b -> PipeM a b m r     -- Yield a value and next status
  Await   : (Maybe a -> PipeM a b m r) -> PipeM a b m r   -- Yield a continuation (expecting a value)

-- Public operations inside a coroutine
-- * `yield` sends a value downstream
-- * `await` waits from an upstream value

export
yield : b -> PipeM a b m ()
yield = Yield (Pure ())

export
await : PipeM a b m (Maybe a)
await = Await Pure

-- Source cannot `await` any input
public export
Source : (m: Type -> Type) -> (b: Type) -> Type
Source m b = PipeM Void b m ()

-- Pipe can `await` and `yield`
public export
Pipe : (a: Type) -> (m: Type -> Type) -> (b: Type) -> Type
Pipe a m b = PipeM a b m ()

-- Sink cannot `yield` anything
public export
Sink : (a: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Sink a m r = PipeM a Void m r

-- Effect cannot `await` or `yield` (pure effect)
public export
Effect : (m: Type -> Type) -> (r: Type) -> Type
Effect m r = PipeM Void Void m r

-- Functor implementation:
-- * Recursively replace `r` with `f r`

export
implementation (Monad m) => Functor (PipeM a b m) where
  map f = assert_total recur where
    recur (Pure r) = Pure (f r)
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (recur . cont)

-- Applicative implementation
-- * Recursively replace `r` with `map r pa`

export
implementation (Monad m) => Applicative (PipeM a b m) where
  pure = Pure
  pf <*> pa = assert_total (recur pf) where
    recur (Pure f) = map f pa
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (recur . cont)

-- Monad implementation
-- * Recursively replace `r` with `f r`

export
implementation (Monad m) => Monad (PipeM a b m) where
  m >>= f = assert_total (recur m) where
    recur (Pure r) = f r
    recur (Action a) = Action (a >>= \x => pure (recur x))
    recur (Yield next b) = Yield (recur next) b
    recur (Await cont) = Await (recur . cont)

-- Monad Transformer implementation
-- * Wrap the monadic action in a `Action` constructor
-- * Wrap the monadic return in a `Pure` constructor

export
implementation MonadTrans (PipeM a b) where
  lift m = assert_total $ Action (m >>= \x => pure (Pure x))

-- Assembling pipes (forms a Category)
-- * Recursively defined to iterate the action of the pipe
-- * Pull-based behavior

infixl 9 .|
infixr 9 >~

mutual

  export
  (.|) : (Monad m) => PipeM a b m r1 -> PipeM b c m r2 -> PipeM a c m r2
  (.|) up (Yield next c) = Yield (up .| next) c             -- Yielding downstream
  (.|) up (Action a) = lift a >>= \next => up .| next       -- Produce effect downstream
  (.|) up (Await cont) = up >~ cont                         -- Ask upstream for a value
  (.|) up (Pure r) = Pure r

  (>~) : (Monad m) => PipeM a b m r1 -> (Maybe b -> PipeM b c m r2) -> PipeM a c m r2
  (>~) (Await cont) down = Await (\a => cont a >~ down)     -- Awaiting upstream
  (>~) (Action a) down = lift a >>= \next => next >~ down   -- Produce effect upstream
  (>~) (Yield next b) down = next .| down (Just b)          -- Give control downstream
  (>~) (Pure r) down = Pure r .| down Nothing               -- Termination, send Nothing to next


-- Running a Effect
-- * Execute the sequence of effects of the pipe
-- * Return the final value when no effects are remaining

export
runEffect : (Monad m) => Effect m r -> m r
runEffect (Pure r) = pure r                         -- Done executing the pipe, return the result
runEffect (Action a) = a >>= \p => runEffect p      -- Execute the action, run the next of the pipe
runEffect (Yield next b) = absurd b                 -- Cannot happen
runEffect (Await cont) = runEffect (cont Nothing)   -- Cannot happen (TODO: use absurd)

export
runPure : Effect Identity r -> r
runPure = runIdentity . runEffect

-- Consuming a Source
-- * Summarize a set of values into a single output value

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

-- Helpers to build standard pipes
-- * `idP` creates the identity Pipe
-- * `each` lifts a foldable to a Source

export
idP : (Monad m) => Pipe a m a
idP = Await (maybe (Pure ()) (Yield idP))

export
each : (Monad m, Foldable f) => f a -> Source m a
each xs = foldr (\x, p => yield x *> p) (pure ()) xs

export
awaitForever : (Monad m) => (i -> PipeM i o m r) -> Pipe i m o
awaitForever f = recur where
  recur = await >>= maybe (pure ()) (\x => f x *> recur)

--
