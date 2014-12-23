{-# LANGUAGE FlexibleInstances #-}

module Cards.Common.Stringe (Stringe, ravel, unravel, shew) where

import Cards.Common

class Stringe a where
    ravel :: String -> a
    unravel :: a -> String
    shew :: a -> String
    shew = show.unravel

instance Stringe String where
    ravel = id
    unravel = id
    shew = show

instance Stringe Keyword where
    ravel x = Keyword x
    unravel (Keyword x) = x

instance Stringe Text where
    ravel x = Text x
    unravel (Text x) = x

instance Show Keyword where
    show = shew
instance Show Text where
    show = shew
