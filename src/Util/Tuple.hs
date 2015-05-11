module Util.Tuple where

import Data.Function
import Control.Monad
import Control.Arrow hiding ((+++))

infixr 5 +++

(+++) :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
(d,e,f)+++(g,h,i) = (d++g,e++h,f++i)

dup :: a -> (a,a)
dup x = (x,x)

tmup :: (a -> b) -> (a, a) -> (b, b)
tmup = zmap.dup

fsts :: ((a,b),(c,d)) -> (a,c)
--fsts (x,y) = (fst x, fst y)
fsts = fst *** fst

snds :: ((a,b),(c,d)) -> (b,d)
--snds (x,y) = (snd x, snd y)
snds = snd *** snd

zap :: ((a -> b),a) -> b
zap = uncurry ($)

tap :: (a -> b, a -> c) -> a -> (b, c)
--tap (f, g) x = (f x, g x)
tap = flip (flip (uncurry (***)). dup)

knock1 :: (a0 -> b, a0 -> c) -> a0 -> (b, c)
knock1 = tap

knock2 :: (a0 -> a1 -> b, a0 -> a1 -> c) -> a0 -> a1 -> (b, c)
knock2 = (tap.).knock1

knock3 :: (a0 -> a1 -> a2 -> b, a0 -> a1 -> a2 -> c) -> a0 -> a1 -> a2 -> (b, c)
knock3 = ((tap.).).knock2

knock4 :: (a0 -> a1 -> a2 -> a3 -> b, a0 -> a1 -> a2 -> a3 -> c) -> a0 -> a1 -> a2 -> a3 -> (b, c)
knock4 = (((tap.).).).knock3

knock5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> (b, c)
knock5 = ((((tap.).).).).knock4

knock6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> (b, c)
knock6 = (((((tap.).).).).).knock5

knock7 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> (b, c)
knock7 = ((((((tap.).).).).).).knock6

zmap :: ((x -> x1),(y -> y1)) -> (x,y) -> (x1,y1)
zmap = curry (tap (zap.fsts,zap.snds))

trefoil :: (a, b, c) -> (a, (b, c))
trefoil (x, y, z) = (x, (y, z))

unfoil :: (a, (b, c)) -> (a, b, c)
unfoil (x,(y,z)) = (x, y, z)

mhall :: ((x -> x1),(y -> y1),(z -> z1)) -> (x,y,z) -> (x1,y1,z1)
--mhall (f,g,h) (a,b,c) = (f a, g b, h c)
--mhall (f,g,h) (a,b,c) = unfoil (f a, (g b, h c))
--mhall (f,g,h) (a,b,c) = unfoil (zmap (f, zmap (g, h)) (a, (b, c)))
--mhall fs@(f,g,h) as@(a,b,c) = unfoil (zmap (zmap (id, zmap) (trefoil fs)) (trefoil as))
mhall = (unfoil.).(.trefoil).zmap.zmap(id,zmap).trefoil
