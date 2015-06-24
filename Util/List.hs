module Util.List where

import Util.Conditional

infix 9 !?
infix 9 ??
infixl 9 !@

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

headDef :: a -> [a] -> a
headDef x [] = x
headDef _ (x:_) = x

neck = (!!1)
body = (!!2)

mmap :: (a -> b) -> Maybe [a] -> [b]
mmap = (?/).map

(!@) :: [a] -> [Int] -> [a]
[]!@i = []
x!@[] = []
(x:xs)!@(i:is)
  | i == 0 = x:xs!@map pred is
  | i <  0 = []
  | otherwise = xs!@map pred is
