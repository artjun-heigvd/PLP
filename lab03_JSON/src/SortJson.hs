{-
    This module arranges the keys of JSON objects in in alphabetical order.

    Authors: Edwin Häffner, Arthur Junod
-}

module SortJson where

    import JsonType
    import Data.List
    import Data.Ord
    import Control.Arrow
    
    sortJsonKeys :: JsonVal -> JsonVal
    sortJsonKeys json = case json of
        -- Sort by comparing the first elem of the pair in JsonObject (the keys in JSON) and calls it recursively on the children
        JsonObject pairs -> JsonObject (sortBy (comparing fst) (map (second sortJsonKeys) pairs))
        JsonArray arr -> JsonArray (map sortJsonKeys arr)
        other -> other