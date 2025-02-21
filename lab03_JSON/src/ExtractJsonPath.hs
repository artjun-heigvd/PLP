{-
    This module extracts specific values from a JSON file using a JSON path expression.
        - A JSON path expression is a way to specify a location within a JSON document to retrieve
    specific values.
        - The minimum operators of JSON path expressions that should be supported are:
            - $: The root element.
            - *: Matches all elements.
            - .<name>: Accesses a child element by name.
            - [<number>]: Accesses an element in an array by its index.

    Authors: Edwin Häffner, Arthur Junod
-}
module ExtractJsonPath (extractPath) where

    import JsonType
    import Data.Char
    import Data.Maybe
    import Text.Read

    -- Exctract the JsonVal that result from the application of the given JSON Path on the given JsonVal
    extractPath :: JsonVal -> String -> JsonVal
    extractPath json path = extractPathAcc (parsePath path) json
        where
            extractPathAcc :: [(String, String)] -> JsonVal -> JsonVal
            extractPathAcc [] jsonVal = jsonVal
            extractPathAcc ((".", val) : rest) (JsonObject obj) =
                case lookup val obj of
                    Just nextJson -> extractPathAcc rest nextJson
                    Nothing -> error $ "Key \"" ++ val ++ "\" not found in object"
            extractPathAcc (("[]", index) : rest) (JsonArray arr) =
                let idx = read index :: Int
                in if idx >= 0 && idx < length arr
                   then extractPathAcc rest (arr !! idx)
                   else error $ "Index " ++ index ++ " out of bounds for array"
            extractPathAcc (("*", _) : rest) (JsonObject obj) =
                JsonArray $ [(`extractPathAcc` JsonObject obj) ((".", key) : rest) |
                               (key, _) <- obj]
            extractPathAcc (("*", _) : rest) (JsonArray arr) =
                JsonArray $ map (extractPathAcc rest) arr
            extractPathAcc _ _ = error "Invalid JSON path or structure"


    -- Allow us to parse the JSON Path given into a list of token that contains (operator, value associated with it)
    parsePath :: String -> [(String, String)]
    parsePath (first:other)
        | first == '$' = parsePathAcc other []
        | otherwise = error "JSON path expressions must start with \'$\'"
        where
            checkNum :: String -> Bool
            checkNum n = isJust (readMaybe n :: Maybe Int)
            parsePathAcc :: String -> [(String, String)] -> [(String, String)]
            parsePathAcc p acc = case p of
                ('.':rest) -> if rest == "" || head rest `elem` ".[]"  then  error $ "JSON path is not valid missing name after a \'.\', state of parsing so far: " ++ show (reverse acc) else
                    let (name, next) = break (`elem` ".[]") rest in
                    case name of
                        "*" -> reverse (("*", "") : acc)
                        _ -> parsePathAcc next ((".", name) : acc)
                ('[':rest) -> let (num, next) = break (==']') rest in
                    if next /= "" && checkNum num then parsePathAcc (tail next) (("[]", num) : acc) else error $ "JSON path is not valid check you closed an open \'[\' and put a number in the index, state of parsing so far: " ++ show (reverse acc)
                "" -> reverse acc
                _ -> error "JSON path is not valid"



