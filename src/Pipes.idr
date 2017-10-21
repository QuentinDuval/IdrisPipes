module Pipes


{-
Main data type for Pipes:
*
-}
data Pipe : (a, b : Type) -> (m : Type -> Type) -> (r : Type) -> Type where
  Pure    : r -> Pipe a b m r                     -- Lift a value into the pipe
  Action  : m (Pipe a b m r) -> Pipe a b m r      -- Wrap a monadic action
  Yield   : b -> Pipe a b m r -> Pipe a b m r     -- Yield a value and next status
  Await   : (a -> Pipe a b m r) -> Pipe a b m r   -- Yield a continuation (expecting a value)

-- Consumer cannot `yield` anything
Consumer : (a: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Consumer a m r = Pipe a Void m r

-- Source cannot `await` any input
Source : (b: Type) -> (m: Type -> Type) -> (r: Type) -> Type
Source b m r = Pipe Void b m r

-- Functor implementation:
-- * Recursively replace `r` with `f r`

implementation (Monad m) => Functor (Pipe a b m) where
  map f p = recur p where
    recur (Pure r) = Pure (f r)
    recur (Action a) = Action (a >>= \p => pure (recur p))
    recur (Yield b next) = Yield b (recur next)
    recur (Await cont) = Await (\a => recur (cont a))

-- Applicative implementation
-- * Recursively replace `r` with `map r pa`

implementation (Monad m) => Applicative (Pipe a b m) where
  pure = Pure
  pf <*> pa = recur pf where
    recur (Pure f) = map f pa
    recur (Action a) = Action (a >>= \p => pure (recur p))
    recur (Yield b next) = Yield b (recur next)
    recur (Await cont) = Await (\a => recur (cont a))

-- Monad implementation
-- * Recursively replace `r` with `f r`

implementation (Monad m) => Monad (Pipe a b m) where
  m >>= f = recur m where
    recur (Pure r) = f r
    recur (Action a) = Action (a >>= \p => pure (recur p))
    recur (Yield b next) = Yield b (recur next)
    recur (Await cont) = Await (\a => recur (cont a))

--
