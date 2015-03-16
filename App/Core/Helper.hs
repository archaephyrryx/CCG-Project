module App.Core.Helper where

import Data.Function
import Control.Monad
import Control.Arrow hiding ((+++))
import Data.Maybe
import Control.Applicative
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as UI
import Reactive.Threepenny

infix 9 ?
infix 9 !?
infix 9 ??
infix 9 ?<
infix 9 ?+
infix 9 ?:
infixr 5 +++

full = not . null

pss :: Show a => Behavior (a -> UI Element)
pss = pure (string . show)

psss :: (Show a, Enum a) => Behavior (a -> UI Element)
psss = pure (string . show . succ)

plss :: Show a => Behavior (a -> UI Element)
plss = pure (estring UI.li . show)

noop :: UI Element
noop = UI.a

estring :: UI Element -> String -> UI Element
estring el = (el #+).(:[]).string

bstring = estring UI.bold
istring = estring UI.italics

filtrate :: [Bool] -> [a] -> [a]
filtrate [] _ = []
filtrate _ [] = []
filtrate (p:ps) (x:xs) = (p?:(x:)) $ filtrate ps xs


afor :: (Applicative f) => f Bool -> f Bool -> f Bool
afor = liftA2 (||)

(!?) :: (a -> b) -> b -> Int -> ([a] -> b)
(f!?y) n xs | n == -1 || n >= length xs = y
            | otherwise = f (xs!!n)

(?) :: (a -> (b -> b)) -> Maybe a -> (b -> b)
f?Nothing = id
f?(Just x) = f x

(??) :: ([a] -> (b -> b)) -> [a] -> (b -> b)
f??[]=id
f??x =f x

(?<) :: (a -> a) -> Int -> (a -> a)
f?<0 = f
f?<_ = id

(?+) :: (a -> a) -> Int -> (a -> a)
f?+1 = id
f?+_ = f

(?:) :: Bool -> (a -> a) -> (a -> a)
p?:f = if p then f else id

(+++) :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
(d,e,f)+++(g,h,i) = (d++g,e++h,f++i)

cdiv :: Int -> Int -> Int
x`cdiv`y | x`mod`y == 0 = x`div`y
         | otherwise = x`div`y + 1

for :: [a] -> (a -> b) -> [b]
for = flip map

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond p f g = \x -> if p x then f x else g x

if_ :: Bool -> a -> a -> a
if_ True x _ = x
if_ False _ y = y

settext = set text

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

mpair :: Monad m => (m a, m b) -> m (a,b)
mpair (mx, my) = do { x <- mx; y <- my; return (x, y) }

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
