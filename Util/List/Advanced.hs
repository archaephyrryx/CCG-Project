{-# LANGUAGE PostfixOperators #-}
module Util.List.Advanced (permute, permutes) where

import Prelude hiding ((!!))
import Data.List (genericIndex)

infixl 9 !!

(!!) :: Integral i => [a] -> i -> a
(!!) = genericIndex

(!) :: Integral a => a -> a
(!) = (map fact [0..] !!)
  where
    fact :: Integral a => a -> a
    fact 0 = 0
    fact i = i * ((i-1) !)

omit :: Integral i => i -> [a] -> [a]
omit _ [] = []
omit 0 (_:xs) = xs
omit i (x:xs) = x:omit (i-1) xs

shuffle :: Integral i => i -> i -> ([a] -> [a])
shuffle 0 _ = id
shuffle n i
  | q == 0 = \(x:xs) -> x:shuffle m r xs
  | otherwise = \xs -> (cycle xs !! q):shuffle m r (omit q xs)
    where
      m = n-1
      (q,r) = i `quotRem` (m !)

permute :: Integral i => i -> [a] -> [a]
permute i x = let l = fromIntegral $ length x
               in shuffle (l !) i x

splices :: a -> [a] -> [[a]]
splices x [] = [[x]]
splices x (y:ys) = (x:y:ys) : map (y:) (splices x ys)

permutes :: [a] -> [[a]]
permutes [] = [[]]
permutes (x:xs) = concat $ map (splices x) $ permutes xs
