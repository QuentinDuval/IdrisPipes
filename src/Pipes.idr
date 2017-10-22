module Pipes

-- import Control.Category
import Control.Monad.Trans

%access public export

{-
Main data type for Pipes:
* not really a `codata`, does not really need `Inf`
* but most function will need `assert_total`
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

-- Source cannot `await` any input
Source : (b: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Source b m r = Pipe Void b m r

-- Sink cannot `yield` anything
Sink : (a: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Sink a m r = Pipe a Void m r

-- Effect cannot `await` or `yield` (pure effect)
Effect : (m: Type -> Type) -> (r: Type) -> Type
Effect m r = Pipe Void Void m r

-- Functor implementation:
-- * Recursively replace `r` with `f r`

implementation (Monad m) => Functor (Pipe a b m) where
  map f p = assert_total $ case p of
    Pure r => Pure (f r)
    Action a => Action (a >>= pure . map f)
    Yield b next => Yield b (map f next)
    Await cont => Await (map f . cont)

-- Applicative implementation
-- * Recursively replace `r` with `map r pa`

implementation (Monad m) => Applicative (Pipe a b m) where
  pure = Pure
  pf <*> pa = assert_total $ case pf of
    Pure f => map f pa
    Action a => Action (a >>= pure . (<*> pa))
    Yield b next => Yield b (next <*> pa)
    Await cont => Await (\a => cont a <*> pa)

-- Monad implementation
-- * Recursively replace `r` with `f r`

implementation (Monad m) => Monad (Pipe a b m) where
  m >>= f = assert_total $ case m of
    Pure r => f r
    Action a => Action (a >>= pure . (>>= f))
    Yield b next => Yield b (next >>= f)
    Await cont => Await (\a => cont a >>= f)

-- Monad Transformer implementation
-- * Wrap the monadic action in a `Action` constructor
-- * Wrap the monadic return in a `Pure` constructor

implementation MonadTrans (Pipe a b) where
  lift m = assert_total $ Action (m >>= pure . Pure)

-- Assembling pipes (forms a Category)
-- * Recursively defined to iterate the action of the pipe

infixl 9 .|
infixr 9 ~>
infixr 9 >~

mutual

  -- * Pull-based (downstream starts first)

  (~>) : (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
  (~>) up (Yield c next) = Yield c (up ~> next)             -- Yielding downstream
  (~>) up (Action a) = lift a >>= \next => up ~> next       -- Produce effect downstream
  (~>) (Yield b next) (Await cont) = next ~> cont b         -- Take upstream available value
  (~>) up (Await cont) = up >~ Await cont                   -- Ask upstream for a value
  (~>) up (Pure r) = Pure r

  -- * Push-based (upstream starts first)

  (>~) : (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
  (>~) (Await cont) down = Await (\a => cont a >~ down)     -- Awaiting upstream
  (>~) (Action a) down = lift a >>= \next => next >~ down   -- Produce effect upstream
  (>~) (Yield b next) down = Yield b next ~> down           -- Give control downstream
  (>~) (Pure r) down = Pure r

(.|) : (Monad m) => Pipe a b m r -> Pipe b c m r -> Pipe a c m r
(.|) = (~>)

-- Running a Effect
-- * Execute the sequence of effects of the pipe
-- * Return the final value when no effects are remaining

runEffect : (Monad m) => Effect m r -> m r
runEffect (Pure r) = pure r           -- Done executing the pipe, return the result
runEffect (Action a) = a >>= runEffect  -- Execute the action, run the next of the pipe
runEffect (Yield b next) = absurd b                         -- Cannot happen
runEffect (Await cont) = runEffect (Await (\v => absurd v))   -- Cannot happen

-- Consuming a Source
-- * Summarize a set of values into a single output value

fold : (Monad m) => (a -> b -> b) -> b -> Source a m r -> m b
fold f = recur where
  recur acc (Pure _) = pure acc
  recur acc (Action act) = act >>= recur acc
  recur acc (Yield b next) = recur (f b acc) next
  recur acc (Await cont) = runEffect (Await (\v => absurd v)) -- Cannot happen

-- Helper functions to construct pipes more easily
-- * `mapping` lifts a function as a pipe transformation
-- * `filtering` lifts a predicate into a pipe filter

each : (Monad m, Foldable f) => f a -> Source a m ()
each xs = foldr (\x, p => yield x *> p) (pure ()) xs

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

--
