{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Stringent where

import Util.List (one)

class Stringent a where
  stringify :: a -> String
  stringify = const "_"

instance (Show a) => Stringent a where
  stringify = show

instance Stringent Int where
  stringify = show

instance Stringent String where
  stringify = id

instance Stringent Char where
  stringify = one
