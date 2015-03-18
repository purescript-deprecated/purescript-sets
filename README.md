# Module Documentation

## Module Data.Set


#### `Set`

``` purescript
data Set a
```


#### `eqSet`

``` purescript
instance eqSet :: (Eq a) => Eq (Set a)
```


#### `showSet`

``` purescript
instance showSet :: (Show a) => Show (Set a)
```


#### `empty`

``` purescript
empty :: forall a. Set a
```


#### `isEmpty`

``` purescript
isEmpty :: forall a. Set a -> Boolean
```


#### `singleton`

``` purescript
singleton :: forall a. a -> Set a
```


#### `checkValid`

``` purescript
checkValid :: forall a. Set a -> Boolean
```


#### `member`

``` purescript
member :: forall a. (Ord a) => a -> Set a -> Boolean
```


#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> Set a -> Set a
```


#### `delete`

``` purescript
delete :: forall a. (Ord a) => a -> Set a -> Set a
```


#### `toList`

``` purescript
toList :: forall a. Set a -> [a]
```


#### `fromList`

``` purescript
fromList :: forall a. (Ord a) => [a] -> Set a
```


#### `union`

``` purescript
union :: forall a. (Ord a) => Set a -> Set a -> Set a
```


#### `unions`

``` purescript
unions :: forall a. (Ord a) => [Set a] -> Set a
```


#### `difference`

``` purescript
difference :: forall a. (Ord a) => Set a -> Set a -> Set a
```


#### `subset`

``` purescript
subset :: forall a. (Ord a) => Set a -> Set a -> Boolean
```

True if and only if every element in the first set
is an element of the second set

#### `properSubset`

``` purescript
properSubset :: forall a. (Ord a) => Set a -> Set a -> Boolean
```

True if and only if the first set is a subset of the second set
and the sets are not equal

#### `intersection`

``` purescript
intersection :: forall a. (Ord a) => Set a -> Set a -> Set a
```

The set of elements which are in both the first and second set