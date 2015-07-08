{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
    UndecidableInstances #-}
module CCG.Cards.Common.Dimorph (Mapping(..), Dimorph(..), to, from) where

import Util
import Data.List (nub)
import Data.Tuple

infix 5 :<=>:

-- | Data-type representing an invertible mapping of a Dimorph
data Mapping a b = a :<=>: b

-- | The 'survey' function checks to ensure a "Mapping" list is consistent,
-- and generates the two morphisms from the Mapping-list, returning them as a
-- pair of functions
survey :: (Eq a, Eq b) => [Mapping a b] -> (a -> b, b -> a)
survey rs | consistent rs = mapping rs
          | otherwise = error "Inconsistent"

-- | Extracts the LHS from a Mapping
lhs :: Mapping a b -> a
lhs (x :<=>: y) = x

-- | Extracts the RHS from a Mapping
rhs :: Mapping a b -> b
rhs (x :<=>: y) = y

-- | Checks to make sure there is only one Mapping for any element of
-- either the LHS or RHS of a list of Mappings
consistent :: (Eq a, Eq b) => [Mapping a b] -> Bool
consistent ms = let (l,r) = tap (map lhs, map rhs) ms in nub l == l && nub r == r

-- | Resolves a "Mapping" list into the forward- and
-- backward-tranformation functions; if any pattern isn't matched, an
-- error is returned if a function is applied to it at runtime
mapping :: (Eq a, Eq b) => [Mapping a b] -> (a -> b, b -> a)
mapping [] = (const (error "Non-exhaustive to-mapping"), const (error "Non-exhaustive from-mapping"))
mapping (r@(x :<=>: y):rs) = (cond (==x) (const y) (fst (mapping rs)), cond (==y) (const x) (snd (mapping rs)))

-- | Data-type representing a full Dimorphism between two types
data Dimorph a b = Dimorph
             { typeX :: [a] -- ^ Ordered (but not sorted) domain
             , typeY :: [b] -- ^ Ordered (but not sorted) range
             }

-- | Generate the to- and from-functions from a "Dimorph"
dimo :: (Eq a, Eq b) => Dimorph a b -> (a -> b, b -> a)
dimo dm = survey (maps dm)

-- | Generate the "Mapping"-list from a "Dimorph"
maps :: (Eq a, Eq b) => Dimorph a b -> [Mapping a b]
maps dm = zipWith (:<=>:) (typeX dm) (typeY dm)

-- | The @a -> b@ side of a "Dimorph" mapping
to :: (Eq a, Eq b) => Dimorph a b -> (a -> b)
to = fst.dimo

-- | The @b -> a@ side of a "Dimorph" mapping
from :: (Eq a, Eq b) => Dimorph a b -> (b -> a)
from = snd.dimo
