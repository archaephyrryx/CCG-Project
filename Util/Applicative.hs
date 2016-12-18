module Util.Applicative ( module Control.Applicative
                        , module Util.Applicative
                        )  where

import Control.Applicative

infixl 4 <^>

(<^>) :: Applicative f => f (a -> b) -> f a -> f b
f<^>g = ($) <$> f <*> g
