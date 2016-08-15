module Util.Applicative ( module Control.Applicative
                        , module Util.Applicative
                        )  where

import Control.Applicative

infixl 4 <^>

(<^>) :: (x -> a -> b) -> (x -> a) -> x -> b
f<^>g = ($) <$> f <*> g
