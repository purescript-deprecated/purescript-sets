# Module Documentation

## Module Data.Set

### Types

    data Set a


### Type Class Instances

    instance eqSet :: (P.Eq a) => P.Eq (Set a)

    instance showSet :: (P.Show a) => P.Show (Set a)


### Values

    checkValid :: forall a. Set a -> Boolean

    delete :: forall a. (P.Ord a) => a -> Set a -> Set a

    difference :: forall a. (P.Ord a) => Set a -> Set a -> Set a

    empty :: forall a. Set a

    fromList :: forall a. (P.Ord a) => [a] -> Set a

    insert :: forall a. (P.Ord a) => a -> Set a -> Set a

    isEmpty :: forall a. Set a -> Boolean

    member :: forall a. (P.Ord a) => a -> Set a -> Boolean

    singleton :: forall a. a -> Set a

    toList :: forall a. Set a -> [a]

    union :: forall a. (P.Ord a) => Set a -> Set a -> Set a

    unions :: forall a. (P.Ord a) => [Set a] -> Set a