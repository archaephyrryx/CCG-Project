module Util.Conditional where

infix 9 ?
infix 9 ?/
infix 9 ?<
infix 9 ?+
infix 9 ?:
infix 9 .=

boolbit :: Int -> Bool
boolbit 0 = False
boolbit _ = True

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond p f g = \x -> if p x then f x else g x

(?) :: (a -> (b -> b)) -> Maybe a -> (b -> b)
f?Nothing = id
f?(Just x) = f x

(?<) :: (a -> a) -> Int -> (a -> a)
f?<0 = f
f?<_ = id

(?+) :: (a -> a) -> Int -> (a -> a)
f?+1 = id
f?+_ = f

(?:) :: Bool -> (a -> a) -> (a -> a)
p?:f = cond (const p) f id
--p?:f = if p then f else id

(.=) :: Eq b => (a -> b) -> b -> (a -> Bool)
f.=x = (==x).f

if_ :: Bool -> a -> a -> a
if_ True x _ = x
if_ False _ y = y

(?/) :: (a -> [b]) -> Maybe a -> [b]
f?/Nothing = []
f?/(Just x) = f x
