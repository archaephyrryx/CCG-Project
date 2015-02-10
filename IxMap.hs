module IxMap (mapix, mapmix, foldmap) where

import Control.Applicative hiding (empty)
import Data.IxSet
import Data.Monoid
import Data.Data (Data, Typeable)
import Data.Typeable (Typeable)
import Data.Set (Set, fold)
import qualified Data.Set as Set
import qualified Data.List as List

-- A function for monotonic mapping over IxSets
mapmix :: (Indexable a, Typeable a, Ord a) => (a -> a) -> (IxSet a -> IxSet a)
mapmix f = fromSet.Set.mapMonotonic f.toSet

-- A function for non-monotonic mapping over IxSets
mapix :: (Indexable a, Typeable a, Ord a) => (a -> a) -> (IxSet a -> IxSet a)
mapix f = fromSet.Set.map f.toSet

foldmap :: (Ord a, Ord t)
        => (c -> (a, (Set t -> Set t)))
        -> Set t
        -> [c]
        -> ([a], Set t)
foldmap f z s = foldr (\x (s', z') -> let (x', f') = (f x) in (List.insert x' s', f' z')) ([], z) s
