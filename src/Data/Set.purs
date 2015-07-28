-- | This module defines a type of sets as balanced 2-3 trees, based on
-- | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>
-- |
-- | Qualified import is encouraged, so as to avoid name clashes with other modules.

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
    size,
    union,
    unions,
    difference
  ) where
  
import Prelude
  
import qualified Data.Map as M

import Data.Maybe 
import Data.Monoid
import Data.Tuple
import Data.Foldable (Foldable, foldMap, foldl, foldr)

import Data.List (List(..))
  
-- | `Set a` represents a set of values of type `a`
data Set a = Set (M.Map a Unit) 

instance eqSet :: (Eq a) => Eq (Set a) where
  eq (Set m1) (Set m2) = m1 == m2

instance showSet :: (Show a) => Show (Set a) where
  show s = "fromList " ++ show (toList s)

instance ordSet :: (Ord a) => Ord (Set a) where
  compare s1 s2 = compare (toList s1) (toList s2)

instance monoidSet :: (Ord a) => Monoid (Set a) where
  mempty = empty

instance monoidSemigroup :: (Ord a) => Semigroup (Set a) where
  append = union

instance foldableSet :: Foldable Set where
  foldMap f = foldMap f <<< toList
  foldl f x = foldl f x <<< toList
  foldr f x = foldr f x <<< toList

-- | An empty set
empty :: forall a. Set a
empty = Set M.empty

-- | Test if a set is empty
isEmpty :: forall a. Set a -> Boolean
isEmpty (Set m) = M.isEmpty m

-- | Create a set with one element
singleton :: forall a. a -> Set a
singleton a = Set (M.singleton a unit)

-- | Check whether the underlying tree satisfies the 2-3 invariant
-- | 
-- | This function is provided for internal use.
checkValid :: forall a. Set a -> Boolean
checkValid (Set m) = M.checkValid m

-- | Test if a value is a member of a set
member :: forall a. (Ord a) => a -> Set a -> Boolean
member a (Set m) = a `M.member` m

-- | Insert a value into a set
insert :: forall a. (Ord a) => a -> Set a -> Set a
insert a (Set m) = Set (M.insert a unit m)
  
-- | Delete a value from a set
delete :: forall a. (Ord a) => a -> Set a -> Set a
delete a (Set m) = Set (a `M.delete` m)
  
-- | Convert a set to a list
toList :: forall a. Set a -> List a
toList (Set m) = map fst (M.toList m)

-- | Create a set from a list of elements
fromList :: forall a. (Ord a) => List a -> Set a
fromList = foldl (\m a -> insert a m) empty

-- | Find the size of a set
size :: forall a. Set a -> Int
size (Set m) = M.size m

-- | Form the union of two sets
-- | 
-- | Running time: `O(n * log(m))`
union :: forall a. (Ord a) => Set a -> Set a -> Set a
union (Set m1) (Set m2) = Set (m1 `M.union` m2)

-- | Form the union of a collection of sets
unions :: forall a. (Ord a) => List (Set a) -> Set a
unions = foldl union empty

-- | Form the set difference
difference :: forall a. (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = foldl (flip delete) s1 (toList s2)
