{-# LANGUAGE TupleSections #-}

module Util.Tuple.Apply where

import Data.Function
import Control.Monad
import Control.Arrow

infixl 9 .<
infixl 1 $<
infixr 9 >.
infixr 1 >$

fsts :: ((a,b),(c,d)) -> (a,c)
fsts = fst *** fst

snds :: ((a,b),(c,d)) -> (b,d)
snds = snd *** snd

zap :: ((a -> b),a) -> b
zap = uncurry ($)

zmap :: ((x -> x1),(y -> y1)) -> (x,y) -> (x1,y1)
zmap = curry (tap (zap.fsts,zap.snds))

tmup :: (a -> b) -> (a, a) -> (b, b)
tmup = zmap.dup

dup :: a -> (a,a)
dup x = (x,x)

tap :: (a -> b, a -> c) -> a -> (b, c)
tap = flip (flip (uncurry (***)). dup)

(.<) :: (b -> c) -> (a -> b) -> (a -> c)
f.<g = f.g

($<) :: (a -> b) -> (a, c) -> (b, c)
($<) = zmap.(,id)

(>.) :: (a -> b) -> (b -> c) -> (a -> c)
g>.f = f.g

(>$) :: (a, b) -> (b -> c) -> (a, c)
(>$) = flip (zmap.(id,))
