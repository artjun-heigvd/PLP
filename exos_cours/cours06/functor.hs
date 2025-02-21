{-
    Define algebraic data type Dict that describes a dictionnary, that is, a set of key-value pairs

    {'a' -> 1, 'b' -> 2}

    should be

    Entry 'a' 1 (Entry 'b' 2 Empty)

    And instantiate a functor to do fmap
-}

data Dict k v = Entry k v (Dict k v) | Empty

-- {} => Empty
-- {'a' -> 1} => Entry 'a' 1 Empty
-- {'a' -> 1, 'b' -> 2} => Entry 'a' 1 $ Entry 'b' 2 Empty

--fmap (+1) (Entry 'a' 1 (Entry 'b' 2 Empty))

instance Functor (Dict k) where
    fmap :: (a -> b) -> Dict k a -> Dict k b
    fmap f Empty = Empty
    fmap f (Entry k v rest) = Entry k (f v) (fmap f rest)