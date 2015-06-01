{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
    UndecidableInstances #-}
module CCG.Cards.Common.Dimorph (Mapping(..), Dimorph(..), to, from) where

import Util
import Data.List (nub)
import Data.Tuple

infix 5 :<=>:

data Mapping a b = a :<=>: b

survey :: (Eq a, Eq b) => [Mapping a b] -> (a -> b, b -> a)
survey rs | consistent rs = mapping rs
          | otherwise = error "Inconsistent"

lhs :: Mapping a b -> a
lhs (x :<=>: y) = x

rhs :: Mapping a b -> b
rhs (x :<=>: y) = y

consistent :: (Eq a, Eq b) => [Mapping a b] -> Bool
consistent ms = let (l,r) = tap (map lhs, map rhs) ms in nub l == l && nub r == r

mapping :: (Eq a, Eq b) => [Mapping a b] -> (a -> b, b -> a)
mapping [] = (const (error "Non-exhaustive to-mapping"), const (error "Non-exhaustive from-mapping"))
mapping (r@(x :<=>: y):rs) = (cond (==x) (const y) (fst (mapping rs)), cond (==y) (const x) (snd (mapping rs)))

data Dimorph a b = Dimorph { typeX :: [a], typeY :: [b] }

dimo :: (Eq a, Eq b) => Dimorph a b -> (a -> b, b -> a)
dimo dm = survey (maps dm)

maps :: (Eq a, Eq b) => Dimorph a b -> [Mapping a b]
maps dm = zipWith (:<=>:) (typeX dm) (typeY dm)

to :: (Eq a, Eq b) => Dimorph a b -> (a -> b)
to = fst.dimo

from :: (Eq a, Eq b) => Dimorph a b -> (b -> a)
from = snd.dimo
