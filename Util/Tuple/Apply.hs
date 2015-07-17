{-# LANGUAGE TupleSections #-}

module Util.Tuple.Apply (
    -- * Tuple Element Selectors
    fsts, snds,
    -- * Tuple-Functorial Operations
    zap, zmap, tmup, dup, tap,
    -- * Left and Right tuple operators
    -- ** Left-Handed Operators
    -- | '.<' and '$<' are the equivalent of '.' and '$' for the first-element functor on tuples.
    (.<), ($<),
    -- ** Right-Handed Operators
    -- | '>.' and '>$' are the equivalent of '.' and '$' for the second-element functor on tuples.
    (>.), (>$)
) where

import Data.Function
import Control.Monad
import Control.Arrow

infixl 9 .<
infixl 1 $<
infixr 9 >.
infixr 1 >$

-- |'fsts' applies 'fst' to a pair of pairs, returning a pair consisting
-- of their respective first elements
fsts :: ((a,b),(c,d)) -> (a,c)
fsts = fst *** fst

-- |'snds' applies 'snd' to a pair of pairs, returning a pair consisting
-- of their respective second elements
snds :: ((a,b),(c,d)) -> (b,d)
snds = snd *** snd

-- |'zap' is a "zippered application" function for function-value pairs
zap :: ((a -> b),a) -> b
zap = uncurry ($)

-- |'zmap' is a "zippered map" function for a pair of functions and a pair of values
zmap :: ((x -> x1),(y -> y1)) -> (x,y) -> (x1,y1)
zmap = curry (tap (zap.fsts,zap.snds))

-- |'tmup' is a "tuple-map duplicated" function for a single function and a pair
-- of values
tmup :: (a -> b) -> (a, a) -> (b, b)
tmup = zmap.dup

-- |'dup' is a "duplication" function that pairs a value with itself
dup :: a -> (a,a)
dup x = (x,x)

-- |'tap' is a "tuple-map" function for a pair of functions and a single
-- value
tap :: (a -> b, a -> c) -> a -> (b, c)
tap = flip (flip (uncurry (***)). dup)

-- |'.<' is a first-element composition operator, which is syntactic
-- sugar for '.'
--
-- > f.<g = f.g
--
-- prop> f.<id = f
-- prop> id.<f = f
(.<) :: (b -> c) -> (a -> b) -> (a -> c)
f.<g = f.g

-- |'$<' is a first-element application operator, to be used with '.<'
-- 
--  > f$<(x,y) = (f x, y)
--
--  prop> f.<g$<x = f$<(g$<x)
--  prop> id$<x = x
($<) :: (a -> b) -> (a, c) -> (b, c)
($<) = zmap.(,id)

-- |'>.' is a second-element composition operator, which is syntactic
-- sugar for a reversed '.'
--
-- > g>.f = f.g
--
-- prop> id>.f = f
-- prop> f>.id = f
(>.) :: (a -> b) -> (b -> c) -> (a -> c)
g>.f = f.g

-- |'>$' is a second-element application operator, to be used with '>.'
-- 
--  prop> x>$g.f = (x>$g)>$f
--  prop> x>$id = x
(>$) :: (a, b) -> (b -> c) -> (a, c)
(>$) = flip (zmap.(id,))
