--
-- Sets as balanced 2-3 trees
--
-- Based on http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf
--

module Data.Set
  ( Set(),
    empty,
    isEmpty,
    singleton,
    checkValid,
    insert,
    member,
    delete,
    toList,
    fromList,
    union,
    unions,
    difference,
    subset,
    properSubset,
    intersection
  ) where

import qualified Data.Map as M

import Data.Array (map, nub, length)
import Data.Maybe
import Data.Tuple
import Data.Foldable (foldl)

data Set a = Set (M.Map a Unit)

instance eqSet :: (Eq a) => Eq (Set a) where
  (==) (Set m1) (Set m2) = m1 == m2
  (/=) (Set m1) (Set m2) = m1 /= m2

instance showSet :: (Show a) => Show (Set a) where
  show s = "fromList " ++ show (toList s)

empty :: forall a. Set a
empty = Set M.empty

isEmpty :: forall a. Set a -> Boolean
isEmpty (Set m) = M.isEmpty m

singleton :: forall a. a -> Set a
singleton a = Set (M.singleton a unit)

checkValid :: forall a. Set a -> Boolean
checkValid (Set m) = M.checkValid m

member :: forall a. (Ord a) => a -> Set a -> Boolean
member a (Set m) = a `M.member` m

insert :: forall a. (Ord a) => a -> Set a -> Set a
insert a (Set m) = Set (M.insert a unit m)

delete :: forall a. (Ord a) => a -> Set a -> Set a
delete a (Set m) = Set (a `M.delete` m)

toList :: forall a. Set a -> [a]
toList (Set m) = map fst (M.toList m)

fromList :: forall a. (Ord a) => [a] -> Set a
fromList = foldl (\m a -> insert a m) empty

union :: forall a. (Ord a) => Set a -> Set a -> Set a
union (Set m1) (Set m2) = Set (m1 `M.union` m2)

unions :: forall a. (Ord a) => [Set a] -> Set a
unions = foldl union empty

difference :: forall a. (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = foldl (flip delete) s1 (toList s2)

-- | True if and only if every element in the first set
-- | is an element of the second set
subset :: forall a. (Ord a) => Set a -> Set a -> Boolean
subset s1 s2 = isEmpty $ s1 `difference` s2

-- | True if and only if the first set is a subset of the second set
-- | and the sets are not equal
properSubset :: forall a. (Ord a) => Set a -> Set a -> Boolean
properSubset s1 s2 = subset s1 s2 && (s1 /= s2)

-- | The set of elements which are in both the first and second set
intersection :: forall a. (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 =
  s12 `difference` s2'1 `difference` s1'2 where
    s1'2 = s1 `difference` s2
    s2'1 = s2 `difference` s1
    s12 = s1 `union` s2
