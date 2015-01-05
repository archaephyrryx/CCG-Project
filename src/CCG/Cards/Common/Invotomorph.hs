module Cards.Common.Invotomorph ( Rule(..) , Invotomorph(..))
                                 where

import Data.List (nub)

infixr 5 :<->:

data Rule a = a :<->: a

regulate :: (Eq a) => [Rule a] -> (a -> a)
regulate rs | consistent rs = ruling rs
            | otherwise = error "Inconsistent"

lhs :: Rule a -> a
lhs (x :<->: y) = x

rhs :: Rule a -> a
rhs (x :<->: y) = y

consistent :: (Eq a) => [Rule a] -> Bool
consistent rs = let sides = ((map rhs rs)++(map lhs rs)) in (nub sides) == sides

ruling :: (Eq a) => [Rule a] -> (a -> a)
ruling [] = id
ruling (r@(x :<->: y):rs) = (\a -> if a == x then y else (if a == y then x else ((ruling rs)$a)))

class (Eq a) => Invotomorph a where
    classX :: [a]
    classX = map lhs rule
    classY :: [a]
    classY = map rhs rule
    invoto :: (a -> a)
    invoto = regulate rule
    rule :: [Rule a]
    rule = zipWith (:<->:) classX classY
