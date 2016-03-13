{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Dimorph.Alt (
  Bijection(..)
  ) where

import Data.Dimorph.Prim (Mapping, mapping)

class (Eq a, Eq b) => Bijection a b where
  mappings :: [Mapping a b]
  to :: a -> b
  to x = (fst (mapping mappings)) x
  from :: b -> a
  from y = (snd (mapping mappings)) y
