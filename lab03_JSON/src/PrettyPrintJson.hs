{-
    This module formats the JSON data in
    a way that is easy for humans to read.

    Authors: Edwin Häffner, Arthur Junod
-}
module PrettyPrintJson where

    import JsonType

    import Data.List (intercalate)

    prettyStr :: JsonVal -> String
    prettyStr = recursivePretty 0
        where
            recursivePretty depth jsonVal = case jsonVal of
                JsonBool b -> show b
                JsonNumber n -> show n
                JsonNull -> "null"
                JsonArray a -> strJsonArray depth a
                JsonObject o -> case o of 
                    [] -> "{\n}"
                    _ -> "{\n" ++ intercalate ",\n" (map (strJsonVal depth) o) ++ "\n" ++ depthTab depth ++ "}"
                JsonString s -> strJsonString s
            strJsonString str = "\"" ++ str ++ "\""
            strJsonVal depth (str, val) = depthTab (depth + 1) ++ strJsonString str ++ ": " ++ recursivePretty (depth + 1) val
            strJsonArray depth a = "[\n" ++ intercalate ",\n" (map (\x -> depthTab (depth + 1) ++ recursivePretty (depth + 1) x) a) ++ "\n" ++ depthTab depth ++ "]"
            depthTab d = concat $ replicate d "    "
