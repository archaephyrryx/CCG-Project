{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types       #-}
-- | More advanced and specialized list functions, for highly specific applications
module Util.List.Advanced (
  -- | Re-export of Prelude without (!!), in order to avoid conflicts for use of generic integral
  -- (!!) operator defined in this module
  module Prelude,
  -- | Re-export of Data.List without (!!), in order to avoid conflicts for use of generic integral
  -- (!!) operator defined in this module
  module Data.List,
  -- * Permutations
  -- ** Helper functions

  (!!),
  -- | Infix-operator version of `genericIndex`, to allow for `Integer`-indexed lists; for large
  -- factorials, necessary in order to avoid overflow
  (!),
  -- | Postfixable factorial operator (with PostfixOperators language pragma)
  omit,
  -- | Complementary operation to (!!), which omits the indicated index, or preserves the list if
  -- the index is out of range
  --
  -- prop> omit 0 = tail
  -- prop> omit -1 = id
  -- prop> omit n xs = take n xs ++ drop (n+1) xs
  -- ** Permutation functions

  -- | Shuffles a list according to an index for the permutation to perform.
  --
  -- prop> shuffle 0 _ = id
  -- prop> shuffle n i = shuffle n (i`mod`n)
  -- prop> shuffle n i xs = shuffle n i (take n xs) ++ drop n xs
  --
  -- Arithmetic properties:
  -- prop> shuffle n i xs = (xs !! (i `div` ((n-1) !))) : (shuffle (n-1) (i `mod` ((n-1) !)) (omit (i `div` ((n-1) !)) xs))
  --
  -- As there are $n$ choices for the first element of the permutation, and $n!$ total permutations,
  -- there are $(n-1)!$ permutations with any given first element. Therefore, we use $\lfloor
  -- \frac{i}{(n-1)!} \rfloor$ to determine the index into the list we will promote to the head, and
  -- recurse with a decremented $n$ and the residue modulo $(n-1)!$ of @i@ as the permutation
  -- index, and the promoted element removed from the rest of the list.
  --
  -- The first argument $n$ is meant to indicate the length of the list being shuffled, but if it is
  -- smaller, then only the first $n$ elements will be permuted, and the rest will be preserved in
  -- their original order. This function only works if $n$ is less than or equal to the length of
  -- the list in question.
  shuffle,
  -- | Performs a specified permutation on the entire list, using the same permutation indexing scheme as
  -- `shuffle`. Unlike shuffle, all (non-negative) integral arguments are valid, as $n$ is chosen by
  -- to be the length of the list in question:
  --
  -- prop> permute k xs = shuffle (length xs) k xs
  permute,
  -- | creates a list of all "splices" of a single element into a list, in head-to-last order
  --
  -- prop> head . head $ splices x _ = x
  -- prop> last . last $ splices x _ = x
  -- prop> (splices x _ !! n) !! n = x
  splices,
  -- | Constructs a list of all possible permutations of a list through recursive splicing; this is
  -- more efficient than the naive implementation, which maps `permute` over all possible
  -- permutation indices
  permutes,
  -- * Clones
  -- Functions that create lists with structural similarity to their argument
  --
  -- | Produces a function based on a list of "indices of truth"; @verity i@ is a function that
  -- preserves length and is universally quantified on the type of the argument list
  --
  -- prop> or $ verity [] _ = False
  -- prop> and $ verity [0..] _ = True
  -- prop> (verity i xs) !! k = k`elem`i
  verity,
  -- | List-length repetition function on a single value
  --
  -- prop> follow z = map $ const z
  follow,
  -- | "Stuttered" enumeration from 0, with the length of each run determined by the integer at the
  -- same index in the given list
  --
  -- prop> concat $ count is = [0..(sum is - 1)]
  -- prop> map length . count = id
  count
                          ) where

import Prelude hiding ((!!))
import Data.List hiding ((!!))

infixl 9 !!

(!!) :: Integral i => [a] -> i -> a
(!!) = genericIndex

(!) :: Integral a => a -> a
(!) = (map fact [0..] !!)
  where
    fact :: Integral a => a -> a
    fact 0 = 1
    fact i = i * ((i-1) !)

omit :: Integral i => i -> [a] -> [a]
omit _ [] = []
omit 0 (_:xs) = xs
omit i (x:xs) = x:omit (i-1) xs

shuffle :: Integral i => i -> i -> ([a] -> [a])
shuffle 0 _ = id
shuffle n i
  | q == 0 = \a -> case a of { [] -> []; x:xs -> x:shuffle m r xs }
  | otherwise = \xs -> (cycle xs !! q):shuffle m r (omit q' xs)
    where
      m = n-1
      (q,r) = i `quotRem` (m !)
      q' = fromIntegral $ q`mod`n

permute :: Integral i => i -> [a] -> [a]
permute i x = let l = fromIntegral $ length x
               in shuffle l i x

splices :: a -> [a] -> [[a]]
splices x [] = [[x]]
splices x (y:ys) = (x:y:ys) : map (y:) (splices x ys)

permutes :: [a] -> [[a]]
permutes [] = [[]]
permutes (x:xs) = concat $ map (splices x) $ permutes xs

verity :: [Int] -> (forall a. [a] -> [Bool])
i`verity`[] = []
[]`verity`x = follow False x
(i:is)`verity`(x:xs)
  | i == 0 = True : (map pred is)`verity`xs
  | i <  0 = []
  | otherwise = False : (map pred (i:is))`verity`xs

follow :: b -> [a] -> [b]
follow z [] = []
follow z (x:xs) = z:follow z xs

count :: [Int] -> [[Int]]
count = count' 0
  where
    count' :: Int -> [Int] -> [[Int]]
    count' n []     = []
    count' n (i:js) = [n..(n+i-1)] : count' (n+i) js