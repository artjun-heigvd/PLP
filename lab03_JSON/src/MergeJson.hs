{-
    This operation combines multiple JSON files into one, provided
    that they have compatible structures.

    Here we're doing an union on the JSON objects with the same keys

    Authors: Edwin Häffner. Arthur Junod
-}
module MergeJson (mergeJson) where

import JsonType
import Data.List
import qualified Data.Maybe

mergeJson :: JsonVal -> JsonVal -> JsonVal
mergeJson val1 val2 = case (val1, val2) of
    
    (JsonObject obj1, JsonObject obj2) -> JsonObject $ mergeObjects obj1 obj2
    
    (JsonArray arr1, JsonArray arr2) -> JsonArray $ nub (arr1 ++ arr2)
    
    (val1', val2') | val1' == val2' -> val1' -- If the values are the same, return one of them
                   | otherwise -> JsonArray [val1', val2'] -- Else make an array with the two of em

-- Merging two objects
mergeObjects :: [(String, JsonVal)] -> [(String, JsonVal)] -> [(String, JsonVal)]
mergeObjects obj1 obj2 = 
    let allKeys = nub $ map fst obj1 ++ map fst obj2
    in map (mergeKey obj1 obj2) allKeys

-- Merging a key
mergeKey :: [(String, JsonVal)] -> [(String, JsonVal)] -> String -> (String, JsonVal)
mergeKey obj1 obj2 key = 
    case (lookup key obj1, lookup key obj2) of
        (Nothing, Nothing) -> (key, JsonNull)
        (Just v1, Nothing) -> (key, v1)
        (Nothing, Just v2) -> (key, v2)
        (Just v1, Just v2) -> (key, mergeJson v1 v2)