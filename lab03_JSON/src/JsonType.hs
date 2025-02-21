{-
    A JSON data type that allow us to manipulate JSON in our functions

    Authors: Edwin Häffner, Arthur Junod
-}

module JsonType where

    data JsonVal = JsonNull
                    | JsonBool Bool
                    | JsonNumber Double
                    | JsonString String
                    | JsonArray [JsonVal]
                    | JsonObject [(String, JsonVal)]
                    deriving (Show)
                    
    
    instance Eq JsonVal where
        JsonNull == JsonNull = True
        JsonBool b1 == JsonBool b2 = b1 == b2
        JsonNumber n1 == JsonNumber n2 = n1 == n2
        JsonString s1 == JsonString s2 = s1 == s2
        JsonArray a1 == JsonArray a2 = a1 == a2
        JsonObject o1 == JsonObject o2 = o1 == o2
        _ == _ = False

  