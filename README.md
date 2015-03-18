# Module Documentation

## Module Data.Set

#### `Set`

``` purescript
data Set a
```


#### `eqSet`

``` purescript
instance eqSet :: (P.Eq a) => P.Eq (Set a)
```


#### `showSet`

``` purescript
instance showSet :: (P.Show a) => P.Show (Set a)
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
member :: forall a. (P.Ord a) => a -> Set a -> Boolean
```


#### `insert`

``` purescript
insert :: forall a. (P.Ord a) => a -> Set a -> Set a
```


#### `delete`

``` purescript
delete :: forall a. (P.Ord a) => a -> Set a -> Set a
```


#### `toList`

``` purescript
toList :: forall a. Set a -> [a]
```


#### `fromList`

``` purescript
fromList :: forall a. (P.Ord a) => [a] -> Set a
```


#### `union`

``` purescript
union :: forall a. (P.Ord a) => Set a -> Set a -> Set a
```


#### `unions`

``` purescript
unions :: forall a. (P.Ord a) => [Set a] -> Set a
```


#### `difference`

``` purescript
difference :: forall a. (P.Ord a) => Set a -> Set a -> Set a
```


#### `subset`

``` purescript
subset :: forall a. (P.Ord a) => Set a -> Set a -> Boolean
```


#### `properSubset`

``` purescript
properSubset :: forall a. (P.Ord a) => Set a -> Set a -> Boolean
```


#### `intersection`

``` purescript
intersection :: forall a. (P.Ord a) => Set a -> Set a -> Set a
```
