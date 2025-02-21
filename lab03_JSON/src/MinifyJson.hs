
{-
    This operation removes all unnecessary whitespace
    (such as spaces, tabs, and newlines) from the JSON data to reduce its size.
-}

module MinifyJson where

    import JsonType 
    import Data.List

    minifyJson :: JsonVal -> String
    minifyJson j = case j of
            JsonBool b -> if b then "True" else "False"
            JsonNumber n ->  checkIfUselessDecimal n
            JsonNull -> "null"
            JsonArray a -> strJsonArray a
            JsonObject o -> "{" ++ intercalate "," (map strJsonVal o) ++ "}"
            JsonString s ->  strJsonString s
            where
                strJsonString str = "\"" ++ str ++ "\""
                strJsonVal (str, val) = strJsonString str ++ ":" ++ minifyJson val
                strJsonArray a = "[" ++ intercalate "," (map minifyJson a) ++ "]"
                checkIfUselessDecimal n = if n == fromIntegral (floor n) then show (round n) else show n

