module Pipes

-- import Control.Category
import Control.Monad.Trans

-- TODO: Action makes the instance of Functor not total (not strictly positive)

{-
Main data type for Pipes:
*
-}
data Pipe : (a, b : Type) -> (m : Type -> Type) -> (r : Type) -> Type where
  Pure    : r -> Pipe a b m r                     -- Lift a value into the pipe
  Action  : m (Pipe a b m r) -> Pipe a b m r      -- Wrap a monadic action
  Yield   : b -> Pipe a b m r -> Pipe a b m r     -- Yield a value and next status
  Await   : (a -> Pipe a b m r) -> Pipe a b m r   -- Yield a continuation (expecting a value)

-- Public operations inside a coroutine
-- * `yield` sends a value downstream
-- * `await` waits from an upstream value

yield : b -> Pipe a b m ()
yield b = Yield b (Pure ())

await : Pipe a b m a
await = Await Pure

-- Consumer cannot `yield` anything
Consumer : (a: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Consumer a m r = Pipe a Void m r

-- Source cannot `await` any input
Source : (b: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Source b m r = Pipe Void b m r

-- Pipeline cannot `await` or `yield`
Pipeline : (m: Type -> Type) -> (r: Type) -> Type
Pipeline m r = Pipe Void Void m r

-- Functor implementation:
-- * Recursively replace `r` with `f r`

implementation (Monad m) => Functor (Pipe a b m) where
  map f = recur where
    recur (Pure r) = Pure (f r)
    recur (Action a) = Action (a >>= pure . recur)
    recur (Yield b next) = Yield b (recur next)
    recur (Await cont) = Await (recur . cont)

-- Applicative implementation
-- * Recursively replace `r` with `map r pa`

implementation (Monad m) => Applicative (Pipe a b m) where
  pure = Pure
  pf <*> pa = recur pf where
    recur (Pure f) = map f pa
    recur (Action a) = Action (a >>= pure . recur)
    recur (Yield b next) = Yield b (recur next)
    recur (Await cont) = Await (recur . cont)

-- Monad implementation
-- * Recursively replace `r` with `f r`

implementation (Monad m) => Monad (Pipe a b m) where
  m >>= f = recur m where
    recur (Pure r) = f r
    recur (Action a) = Action (a >>= pure . recur)
    recur (Yield b next) = Yield b (recur next)
    recur (Await cont) = Await (recur . cont)

-- Monad Transformer implementation
-- * Wrap the monadic action in a `Action` constructor
-- * Wrap the monadic return in a `Pure` constructor

implementation MonadTrans (Pipe a b) where
  lift m = Action (m >>= pure . Pure)

-- Assembling pipes (forms a Category)
-- * Recursively defined to iterate the action of the pipe

infixl 9 .|

(.|) : (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
(.|) = fuse where
  fuse (Pure r1) _ = Pure r1                                -- Termination of pipeline
  fuse _ (Pure r2) = Pure r2                                -- Termination of pipeline
  fuse (Yield b next) (Await cont) = next .| cont b         -- Connect yield to await
  fuse up (Yield c next) = Yield c (up .| next)             -- Yielding downstream
  fuse (Await cont) down = Await (\a => cont a .| down)     -- Awaiting upstream
  fuse (Action a) down = lift a >>= \next => next .| down   -- Produce effect upstream
  fuse up (Action a) = lift a >>= \next => up .| next       -- Produce effect downstream

-- Running a pipeline
-- * Execute the sequence of effects of the pipe
-- * Return the final value when no effects are remaining

runPipe : (Monad m) => Pipeline m r -> m r
runPipe (Pure r) = pure r           -- Done executing the pipe, return the result
runPipe (Action a) = a >>= runPipe  -- Execute the action, run the next of the pipe
runPipe (Yield b next) = absurd b                         -- Cannot happen
runPipe (Await cont) = runPipe (Await (\v => absurd v))   -- Cannot happen

-- Helper functions to construct pipes more easily
-- * `mapping` lifts a function as a pipe transformation
-- * `filtering` lifts a predicate into a pipe filter

mapping : (Monad m) => (a -> b) -> Pipe a b m r
mapping f = recur where
  recur = do
    a <- await
    yield (f a)
    recur

filtering : (Monad m) => (a -> Bool) -> Pipe a a m r
filtering p = recur where
  recur = do
    a <- await
    if p a
      then yield a *> recur
      else recur

--
