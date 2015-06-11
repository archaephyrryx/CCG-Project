module Util.Helper where

import Control.Monad
import Control.Applicative
import Util.List (one)

-- |The @||@ operator lifted to applicative functors
afor :: (Applicative f) => f Bool -> f Bool -> f Bool
afor = liftA2 (||)

-- |Ceiling-division
cdiv :: Int -> Int -> Int
x`cdiv`y | x`mod`y == 0 = x`div`y
         | otherwise = x`div`y + 1

-- |The 'mpair' function converts paired monads to monadic pairs
mpair :: Monad m => (m a, m b) -> m (a,b)
mpair (mx, my) = do { x <- mx; y <- my; return (x, y) }

-- |The 'wrapped' function wraps values in singleton lists
wrapped :: ([a] -> b) -> (a -> b)
wrapped = (.one)

-- |Converts a normal string-parser into a basic Read(er)
rdr :: (String -> a) -> (ReadS a)
rdr = (wrapped (`zip`[""]) .)
