module Util.List where

import Util.Conditional

infix 9 !?
infix 9 ??

full = not . null

filtrate :: [Bool] -> [a] -> [a]
filtrate [] _ = []
filtrate _ [] = []
filtrate (p:ps) (x:xs) = (p?:(x:)) $ filtrate ps xs

(!?) :: (a -> b) -> b -> Int -> ([a] -> b)
(f!?y) n xs | n == -1 || n >= length xs = y
            | otherwise = f (xs!!n)

(??) :: ([a] -> (b -> b)) -> [a] -> (b -> b)
f??[]=id
f??x =f x

for :: [a] -> (a -> b) -> [b]
for = flip map

one :: a -> [a]
one = (:[])

once :: (a -> b) -> (a -> [b])
once = (one.)

