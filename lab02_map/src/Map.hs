module Map where
import Data.Maybe (isJust)
import Data.Char (chr, toUpper, isNumber, ord)
import Data.List (intercalate, sort)


type Map = Char -> Maybe Int

instance Show Map where
    show map = "{" ++ intercalate ", " (toListStrAll (toList map)) ++ "}"
        where 
            toListStrAll mapList = case mapList of
                [] -> []
                ((key, value):tail) -> (show key ++ " -> " ++ show value) : toListStrAll tail

instance Eq Map where
    mapl == mapr = sort  (toList mapl) == sort (toList mapr)

m :: Char -> Maybe Int
m 'a' = Just 1
m 'b' = Just 2
m 'c' = Just 3
m _ = Nothing

-- | Empty map
empty :: Map
empty = \_ -> Nothing

-- | Singleton map
singleton :: Char -> Int -> Map
singleton key value = \x -> if x == key then Just value else Nothing

-- | Insert a key-value pair into the map
insert :: Char -> Int -> Map -> Map
insert key value map = \x -> if x == key then Just value else map x

-- | Find the value associated with a key in the map
find :: Char -> Map -> Maybe Int
find key map = map key

-- | Find the value associated with a key in the map or return a default value
findOrDefault :: Char -> Int -> Map -> Int
findOrDefault key defValue map = 
    case find key map of 
        Nothing -> defValue
        Just value -> value

-- | Delete a key from the map
delete :: Char -> Map -> Map
delete key map = \x -> if x == key then Nothing else map x


-- | Update the value associated with a key in the map
update :: Char -> (Int -> Int) -> Map -> Map
update key func map = \x -> if x == key then 
    case map x of 
        Just value -> Just (func value)
        Nothing -> Nothing 
    else map x 

-- | Check if a key exists in the map
member :: Char -> Map -> Bool
member key map = 
    case map key of
        Just _ -> True
        Nothing -> False
        

-- | Get the size of the map
-- Il faut brute force
size :: Map -> Int
size map = length $ keys map

-- | Check if the map is empty
isEmpty :: Map -> Bool
isEmpty map = size map == 0

-- | Get all keys in the map
keys :: Map -> [Char]
keys map = [chr char | char <- [0 .. 127], isJust (map (chr char))]

-- | Get all values in the map
values :: Map -> [Int]
values map = [val | char <- [0..127], Just val <- [map (chr char)]]

-- | Get the keys associated with a given value
keysWithValue :: Int -> Map -> [Char]
keysWithValue val map = [chr char | char <- [0..127], Just comp <- [map(chr char)], comp == val]

-- | Union of two maps
-- If the key is the same but the values are different, take the value of the first map
union :: Map -> Map -> Map
union mapl mapr = \x -> case () of
                      _ | member x mapl -> mapl x
                        | member x mapr -> mapr x
                        | otherwise -> Nothing


-- | Intersection of two maps
intersection :: Map -> Map -> Map
intersection mapl mapr = \x -> case () of
                         _ | member x mapl && member x mapr -> mapl x
                           | otherwise -> Nothing

-- | Difference of two maps
difference :: Map -> Map -> Map
difference mapl mapr = \x -> case () of
             _ | member x mapl && member x mapr -> Nothing
               | member x mapl -> mapl x
               | otherwise -> Nothing

-- | Apply a function to all values in the map
mapValues :: (Int -> Int) -> Map -> Map
mapValues func map = \x -> case map x of
                           Just value -> Just (func value)
                           Nothing -> Nothing

-- | Apply a function to all keys in the map
-- Si x est egal à une func key, alors retourner map keys
mapKeys :: (Char -> Char) -> Map -> Map
mapKeys func map = fromList [(func key, val) | (key, val) <- toList map]

-- | Filter the map by a predicate
filterMap :: (Char -> Int -> Bool) -> Map -> Map
filterMap pred map = \x -> case map x of
                            Just value -> if pred x value then Just value else Nothing
                            Nothing -> Nothing

-- | Check if any entry in the map satisfies a predicateb
anyEntry :: (Char -> Int -> Bool) -> Map -> Bool
anyEntry pred map = size (filterMap pred map) /= 0

-- | Check if all entries in the map satisfy a predicate
allEntries :: (Char -> Int -> Bool) -> Map -> Bool
allEntries pred map = size (filterMap pred map) == size map

-- | Partition the map into two maps based on a predicate
partition :: (Char -> Int -> Bool) -> Map -> (Map, Map)
partition pred map = (filterMap pred map, filterMap notPred map)
    where notPred key val = not (pred key val)

-- | Split the map into two maps at a given key
split :: Char -> Map -> (Map, Map)
split key map = (mapl, mapr)
    where mapl = \x -> if x < key then map x else Nothing
          mapr = \x -> if x >= key then map x else Nothing

-- | Get the maximum value in the map
findMax :: Map -> Maybe Int
findMax map = if null vals then Nothing else Just (maximum vals)
    where vals = values map

-- | Get the minimum value in the map
findMin :: Map -> Maybe Int
findMin map = if null vals then Nothing else Just (minimum vals)
            where vals = values map

-- | Convert a list of key-value pairs to a map
fromList :: [(Char, Int)] -> Map
fromList = foldr (uncurry insert) empty

-- | Convert the map to a list of key-value pairs
toList :: Map -> [(Char, Int)]
toList map = [(key, val) | key <- keys map, Just val <- [map key] ]

-- | Deserialize a map from a string
fromString :: String -> Map
fromString strMap = case strMap of
    "{}" -> empty
    ('{':tail) -> fromString tail
    ('\'':key:'\'':' ':'-':'>':' ':value:rest) -> if isNumber value then insert key (read [value]) (fromString rest) else error "Map given has not number values"
    (',':' ':rest) -> fromString rest
    "}" -> empty
    _ -> error "Not the correct format of string for a map"