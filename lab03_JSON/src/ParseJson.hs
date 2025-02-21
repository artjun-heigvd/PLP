{-
    This module expose a function that allow us to parse a JSON from a given String and 
    another that can trim a String into another that is more easily parsable for the first function

    Authors: Edwin Häffner, Arthur Junod
-}


module ParseJson (trimming, parseJson) where 

    import JsonType
    import Data.Char (isSpace)
    
    -- Parse a JSON string into a JsonVal
    parseJson :: String -> JsonVal
    parseJson [] = JsonObject []
    parseJson str =
        let (x:xs) = str in
            case x of
                '{' -> parseObject xs
                '[' -> parseArray xs
                '"' -> parseString xs
                'n' -> if take 4 str == "null"
                    then JsonNull
                    else error "Invalid JSON (null)"
                't' -> if take 4 str == "true"
                    then JsonBool True
                    else error "Invalid JSON (true)"
                'f' -> if take 5 str == "false"
                    then JsonBool False
                    else error "Invalid JSON (false)"
                _   -> if x `elem` "-0123456789"
                    then parseNumber str
                    else error ("Invalid JSON: " ++ [x])
        where
        -- Parse arrays
        parseArray :: String -> JsonVal
        parseArray (']':_) = JsonArray []
        parseArray str = JsonArray $ parseSequence ']' parseJson str
        -- Parse objects
        parseObject :: String -> JsonVal
        parseObject ('}':_) = JsonObject []
        parseObject str = JsonObject $ parseSequence '}' parseKeyValue str
        
        -- Generic sequence parser for both arrays and objects
        parseSequence :: Char -> (String -> a) -> String -> [a]
        parseSequence endChar parser str =
            case parseNext str of
                Nothing -> error "Invalid JSON (sequence)"
                Just (val, rest) ->
                    let parsedVal = parser val
                        rest' = dropWhile (\c -> c == ',' || c == ' ') rest
                    in if null rest' || head rest' == endChar
                    then [parsedVal]
                    else parsedVal : parseSequence endChar parser rest'
            where
                parseNext :: String -> Maybe (String, String)
                parseNext [] = Nothing
                parseNext [x] | x == endChar = Nothing
                parseNext input = Just $ breakJson ',' input

        -- Break the JSON until the delimiter (',' or '}')       
        breakJson :: Char -> String -> (String, String)
        breakJson delimiter = break' 0 False
            where
                break' :: Int -> Bool -> String -> (String, String)
                break' _ _ [] = ("", "")
                break' depth inString (c:cs) = case c of
                    '"' -> nextChar depth (not inString)
                    '{' -> nextChar (if inString then depth else depth + 1) inString
                    '}' -> nextChar (if inString then depth else depth - 1) inString
                    '[' -> nextChar (if inString then depth else depth + 1) inString
                    ']' -> nextChar (if inString then depth else depth - 1) inString
                    x   | x == delimiter && depth == 0 && not inString -> ("", cs)
                    _   -> nextChar depth inString
                    where 
                        nextChar d s = let (x, y) = break' d s cs in (c:x, y)
        -- Parse key-value pairs
        parseKeyValue :: String -> (String, JsonVal)
        parseKeyValue str =
            let str' = dropWhile (== '"') str  -- Skip initial quote
                (key, rest) = break (== '"') str'  -- Take until closing quote
                rest' = dropWhile (\c -> c == ':' || c == ' ') $ dropWhile (== '"') rest  -- Skip quote, colon and spaces
            in (key, parseJson rest')
        -- Parse numbers
        parseNumber :: String -> JsonVal
        parseNumber (x:xs)
            | null xs && x == '-' = error "Invalid JSON (number)"
            | otherwise = JsonNumber $ read $ takeWhile (`elem` "-0123456789.") (x:xs)
        -- Parse strings
        parseString :: String -> JsonVal
        parseString str = JsonString $ takeWhile (/= '"') $ dropWhile (== '"') str


    -- Trim the input string into something more workable
    trimming :: String -> String
    trimming [] = []
    trimming xs = trim' xs False []  -- False becomes true when we're in a string
        where
            trim' [] _ acc = reverse acc
            trim' (x:xs) inString acc
                | x == '"' = trim' xs (not inString) (x : acc)
                | inString || notElem x " \t\n" = trim' xs inString (x : acc) --Keep everything if we're in a string !
                | otherwise = trim' xs inString acc -- Remove the space, tabs and newlines
