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
    union,
    unions,
    difference,
    subset,
    properSubset,
    intersection
  ) where
  
import qualified Data.Map as M

import Control.Monad.Eff (runPure, Eff())
import Control.Monad.ST (ST())
import Control.Monad.Rec.Class (tailRecM2)
import Data.Array (map, nub, length)
import Data.Array.ST
import Data.Either
import Data.Maybe 
import Data.Tuple
import Data.Foldable (foldl) 
import Prelude.Unsafe (unsafeIndex)
  
-- | `Set a` represents a set of values of type `a`
data Set a = Set (M.Map a Unit) 

instance eqSet :: (Eq a) => Eq (Set a) where
  (==) (Set m1) (Set m2) = m1 == m2
  (/=) (Set m1) (Set m2) = m1 /= m2

instance showSet :: (Show a) => Show (Set a) where
  show s = "fromList " ++ show (toList s)

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
  
-- | Convert a set to an array
toList :: forall a. Set a -> [a]
toList (Set m) = map fst (M.toList m)

-- | Create a set from an array of elements
fromList :: forall a. (Ord a) => [a] -> Set a
fromList = foldl (\m a -> insert a m) empty

-- | Form the union of two sets
-- | 
-- | Running time: `O(n * log(m))`
union :: forall a. (Ord a) => Set a -> Set a -> Set a
union (Set m1) (Set m2) = Set (m1 `M.union` m2)

-- | Form the union of a collection of sets
unions :: forall a. (Ord a) => [Set a] -> Set a
unions = foldl union empty

-- | Form the set difference
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
intersection s1 s2 = fromList $ runPure (runSTArray (emptySTArray >>= intersect)) where
  ls = toList s1
  rs = toList s2
  ll = length ls
  rl = length rs
  intersect :: forall h r. STArray h a -> Eff (st :: ST h | r) (STArray h a)
  intersect acc = tailRecM2 go 0 0 where
    go l r =
      if l < ll && r < rl
      then case compare (ls `unsafeIndex` l) (rs `unsafeIndex` r) of
        EQ -> do
          pushSTArray acc (ls `unsafeIndex` l)
          pure $ Left {a: l + 1, b: r + 1}
        LT -> pure $ Left {a: l + 1, b: r}
        GT -> pure $ Left {a: l, b: r + 1}
      else pure $ Right acc
