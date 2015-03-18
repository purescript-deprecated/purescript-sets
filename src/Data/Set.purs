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

import qualified Prelude as P

import qualified Data.Map as M

import Data.Array (map, nub, length)
import Data.Maybe
import Data.Tuple
import Data.Foldable (foldl)

data Set a = Set (M.Map a P.Unit)

instance eqSet :: (P.Eq a) => P.Eq (Set a) where
  (==) (Set m1) (Set m2) = m1 P.== m2
  (/=) (Set m1) (Set m2) = m1 P./= m2

instance showSet :: (P.Show a) => P.Show (Set a) where
  show s = "fromList " P.++ P.show (toList s)

empty :: forall a. Set a
empty = Set M.empty

isEmpty :: forall a. Set a -> Boolean
isEmpty (Set m) = M.isEmpty m

singleton :: forall a. a -> Set a
singleton a = Set (M.singleton a P.unit)

checkValid :: forall a. Set a -> Boolean
checkValid (Set m) = M.checkValid m

member :: forall a. (P.Ord a) => a -> Set a -> Boolean
member a (Set m) = a `M.member` m

insert :: forall a. (P.Ord a) => a -> Set a -> Set a
insert a (Set m) = Set (M.insert a P.unit m)

delete :: forall a. (P.Ord a) => a -> Set a -> Set a
delete a (Set m) = Set (a `M.delete` m)

toList :: forall a. Set a -> [a]
toList (Set m) = map fst (M.toList m)

fromList :: forall a. (P.Ord a) => [a] -> Set a
fromList = foldl (\m a -> insert a m) empty

union :: forall a. (P.Ord a) => Set a -> Set a -> Set a
union (Set m1) (Set m2) = Set (m1 `M.union` m2)

unions :: forall a. (P.Ord a) => [Set a] -> Set a
unions = foldl union empty

difference :: forall a. (P.Ord a) => Set a -> Set a -> Set a
difference s1 s2 = foldl (P.flip delete) s1 (toList s2)

subset :: forall a. (P.Ord a) => Set a -> Set a -> Boolean
subset s1 s2 = isEmpty P.$ s1 `difference` s2

properSubset :: forall a. (P.Ord a) => Set a -> Set a -> Boolean
properSubset s1 s2 = subset s1 s2 P.&& (P.not P.<<< isEmpty P.$ s2 `difference` s1)

intersection :: forall a. (P.Ord a) => Set a -> Set a -> Set a
intersection s1 s2 =
  s12 `difference` s2'1 `difference` s1'2 where
    s1'2 = s1 `difference` s2
    s2'1 = s2 `difference` s1
    s12 = s1 `union` s2
