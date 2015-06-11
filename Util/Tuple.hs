module Util.Tuple ( (+++)
                  , knock1, knock2, knock3, knock4, knock5, knock6, knock7
                  , trefoil, unfoil
                  , mhall
                  , swap
                  , module Util.Tuple.Apply
                  ) where

import Data.Function
import Control.Monad
import Util.Tuple.Apply

infixr 5 +++

-- |'swap' reverses the order of a pair
swap :: (a,b) -> (b,a)
swap = uncurry (flip (,))

-- |Concatenates corresponding items in a list 3-tuple
(+++) :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
(d,e,f)+++(g,h,i) = (d++g,e++h,f++i)

knock1 :: (a0 -> b, a0 -> c) -> a0 -> (b, c)
knock1 = tap
knock2 :: (a0 -> a1 -> b, a0 -> a1 -> c) -> a0 -> a1 -> (b, c)
knock2 = (tap.).knock1
knock3 :: (a0 -> a1 -> a2 -> b, a0 -> a1 -> a2 -> c) -> a0 -> a1 -> a2 -> (b, c)
knock3 = ((tap.).).knock2
knock4 :: (a0 -> a1 -> a2 -> a3 -> b, a0 -> a1 -> a2 -> a3 -> c) -> a0 -> a1 -> a2 -> a3 -> (b, c)
knock4 = (((tap.).).).knock3
knock5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> (b, c)
knock5 = ((((tap.).).).).knock4
knock6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> (b, c)
knock6 = (((((tap.).).).).).knock5
knock7 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b, a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> c) -> a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> (b, c)
knock7 = ((((((tap.).).).).).).knock6

-- |'trefoil' \'folds\' a 3-tuple into an item-pair pair
trefoil :: (a, b, c) -> (a, (b, c))
trefoil (x, y, z) = (x, (y, z))

-- |'unfoil' \'unfolds\' an item-pair pair into a 3-tuple
unfoil :: (a, (b, c)) -> (a, b, c)
unfoil (x,(y,z)) = (x, y, z)

-- |'mhall' maps a triplet of functions over a triplet of values
mhall :: ((x -> x1),(y -> y1),(z -> z1)) -> (x,y,z) -> (x1,y1,z1)
mhall = (unfoil.).(.trefoil).zmap.zmap(id,zmap).trefoil
