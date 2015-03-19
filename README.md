# Module Documentation

Efficient sets implemented via Data.Map (i.e., purescript-maps).  The underlying data structure is a balanced 2-3 tree.  (See, e.g., http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf)

## Usage

Qualified import is encouraged, so as to avoid name clashes with other modules.

``` purescript
import qualified Data.Set as Set
```

As usual, installation is performed with Bower.

```
bower install purescript-sets -S
```

## Module Data.Set

##### `Set`
A set of values of type ```a```.
``` purescript
data Set a
```


##### `eqSet`
Sets containing values of type ```a``` can be compared for equality provided that values of type ```a``` are members of the Prelude's ```Eq``` class.
``` purescript
instance eqSet :: (P.Eq a) => P.Eq (Set a)
```


##### `showSet`
Sets containing values of type ```a``` have a string representation provided that a value of type ```a``` is a member of the Prelude's ```Show``` class.
``` purescript
instance showSet :: (P.Show a) => P.Show (Set a)
```


##### `empty`
The value of the empty set.
``` purescript
empty :: forall a. Set a
```


##### `isEmpty`
Takes a set as an argument and returns ```true``` if the set is the ```empty``` value; otherwise, returns ```false```.
``` purescript
isEmpty :: forall a. Set a -> Boolean
```


##### `singleton`
Takes a value and creates a set that contains only that value.
``` purescript
singleton :: forall a. a -> Set a
```


##### `checkValid`
Tests whether the internal structure of a set is valid.  That is, ```checkValid``` takes a set and determines whether its "2-3 tree" representation is balanced, ordered, and satisfies the path-length invariant. 
``` purescript
checkValid :: forall a. Set a -> Boolean
```


##### `member`
Takes a value of type ```a``` and a set of ```a``` and returns ```true``` if the value is an element of the set.
``` purescript
member :: forall a. (P.Ord a) => a -> Set a -> Boolean
```


##### `insert`
Takes a value of type ```a```, and an existing set of ```a```, and returns a new set comprising the elements of the existing set and the new value.
``` purescript
insert :: forall a. (P.Ord a) => a -> Set a -> Set a
```


##### `delete`
Takes a value of type ```a```, and an existing set of ```a```, and returns a new set comprising the elements of the existing set excluding the provided value.  If the value is not an element of the existing set, the ```delete``` function simply returns a copy of the existing set.
``` purescript
delete :: forall a. (P.Ord a) => a -> Set a -> Set a
```


##### `toList`
Converts a value of type ```Set``` to a value of type ```Array```.
``` purescript
toList :: forall a. Set a -> [a]
```


##### `fromList`
Converts a value of type ```Array``` to a value of type ```Set```.
``` purescript
fromList :: forall a. (P.Ord a) => [a] -> Set a
```


##### `union`
Takes two existing sets and returns a new set comprising all distinct elements of the two sets.
``` purescript
union :: forall a. (P.Ord a) => Set a -> Set a -> Set a
```


##### `unions`
Takes an array of sets (i.e., a collection of sets) and returns a new set comprising all distinct elements of the collected sets.
``` purescript
unions :: forall a. (P.Ord a) => [Set a] -> Set a
```


##### `difference`
Takes two sets and returns a new set comprising only those elements of the first set (i.e., argumentOneSet) that are not elements of the second set (i.e., argumentTwoSet). 
``` purescript
difference :: forall a. (P.Ord a) => Set a -> Set a -> Set a
```
