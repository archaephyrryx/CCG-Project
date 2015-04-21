module Util.Helper where

import Control.Monad
import Control.Applicative
import Util.List (one)

afor :: (Applicative f) => f Bool -> f Bool -> f Bool
afor = liftA2 (||)

cdiv :: Int -> Int -> Int
x`cdiv`y | x`mod`y == 0 = x`div`y
         | otherwise = x`div`y + 1

mpair :: Monad m => (m a, m b) -> m (a,b)
mpair (mx, my) = do { x <- mx; y <- my; return (x, y) }

wrapped :: ([a] -> b) -> (a -> b)
wrapped = (.one)

rdr :: (String -> a) -> (ReadS a)
rdr = (wrapped (`zip`[""]) .)
