-----------
-- TESTS --
-----------
-- Authors: Edwin Häffner, Arthur Junod

module JsonTest (tests) where 

import Test.HUnit
import JsonType
import PrettyPrintJson
import MinifyJson
import MergeJson
import ParseJson
import ExtractJsonPath
import SortJson

-- Basic object tests
testEmptyObject :: Test
testEmptyObject = TestCase $
    assertEqual "Empty object" (JsonObject []) (parseJson "{}")

testSimpleObject :: Test
testSimpleObject = TestCase $
    assertEqual "Simple object"
        (JsonObject [("name", JsonString "John")])
        (parseJson "{\"name\":\"John\"}")

testMultipleTypes :: Test
testMultipleTypes = TestCase $
    assertEqual "Multiple types"
        (JsonObject [
            ("name", JsonString "John"),
            ("age", JsonNumber 30),
            ("isStudent", JsonBool False)
        ])
        (parseJson "{\"name\":\"John\",\"age\":30,\"isStudent\":false}")

testNestedObject :: Test
testNestedObject = TestCase $
    assertEqual "Nested object"
        (JsonObject [
            ("person", JsonObject [
                ("name", JsonString "John"),
                ("age", JsonNumber 30)
            ])
        ])
        (parseJson "{\"person\":{\"name\":\"John\",\"age\":30}}")

testObjectWithArray :: Test
testObjectWithArray = TestCase $
    assertEqual "Object with array"
        (JsonObject [
            ("numbers", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3]),
            ("name", JsonString "Test")
        ])
        (parseJson "{\"numbers\":[1,2,3],\"name\":\"Test\"}")

testObjectInArray :: Test
testObjectInArray = TestCase $
    assertEqual "Object in array"
        (JsonArray [
            JsonObject [("name", JsonString "John"), ("age", JsonNumber 30)],
            JsonObject [("name", JsonString "Jane"), ("age", JsonNumber 25)]
        ])
        (parseJson "[{\"name\":\"John\",\"age\":30},{\"name\":\"Jane\",\"age\":25}]")

testNullObject :: Test
testNullObject = TestCase $
    assertEqual "Null object"
        JsonNull
        (parseJson "null")

testTrimming :: Test
testTrimming = TestCase $
    assertEqual "Trimming"
        (
            trimming "       {\"name\":\"John\"                ,\"age\"   \n:30}       "
        ) "{\"name\":\"John\",\"age\":30}"

testSort :: Test
testSort = TestCase $ 
    assertEqual "Sort empty object"
        (JsonObject [])
        (sortJsonKeys (JsonObject []))

testSortSimple :: Test
testSortSimple = TestCase $
    assertEqual "Sort simple object"
        (JsonObject [("a", JsonNumber 1), ("b", JsonNumber 2)])
        (sortJsonKeys (JsonObject [("b", JsonNumber 2), ("a", JsonNumber 1)]))

testSortNested :: Test
testSortNested = TestCase $
    assertEqual "Sort nested object"
        (JsonObject [
            ("a", JsonNumber 1),
            ("z", JsonObject [("x", JsonNumber 1), ("y", JsonNumber 2)])
        ])
        (sortJsonKeys (JsonObject [
            ("z", JsonObject [("y", JsonNumber 2), ("x", JsonNumber 1)]),
            ("a", JsonNumber 1)
        ]))

testMergeEmpty :: Test
testMergeEmpty = TestCase $
    assertEqual "Merge empty objects"
        (JsonObject [])
        (mergeJson (JsonObject []) (JsonObject []))

testMergeSimple :: Test
testMergeSimple = TestCase $
    assertEqual "Merge simple objects"
        (JsonObject [("a", JsonNumber 1), ("b", JsonNumber 2)])
        (mergeJson 
            (JsonObject [("a", JsonNumber 1)]) 
            (JsonObject [("b", JsonNumber 2)]))

testMergeArrays :: Test
testMergeArrays = TestCase $
    assertEqual "Merge arrays"
        (JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])
        (mergeJson 
            (JsonArray [JsonNumber 1, JsonNumber 2]) 
            (JsonArray [JsonNumber 2, JsonNumber 3]))

testMergeConflict :: Test
testMergeConflict = TestCase $
    assertEqual "Merge conflicting values"
        (JsonObject [("a", JsonArray [JsonNumber 1, JsonNumber 2])])
        (mergeJson 
            (JsonObject [("a", JsonNumber 1)]) 
            (JsonObject [("a", JsonNumber 2)]))

testMinifyEmpty :: Test
testMinifyEmpty = TestCase $
    assertEqual "Minify empty object"
        "{}"
        (minifyJson (JsonObject []))

testMinifySimple :: Test
testMinifySimple = TestCase $
    assertEqual "Minify simple object"
        "{\"name\":\"John\"}"
        (minifyJson (JsonObject [("name", JsonString "John")]))

testMinifyComplex :: Test
testMinifyComplex = TestCase $
    assertEqual "Minify complex object"
        "{\"name\":\"John\",\"numbers\":[1,2]}"
        (minifyJson (JsonObject [
            ("name", JsonString "John"),
            ("numbers", JsonArray [JsonNumber 1, JsonNumber 2])
        ]))

testExtractRoot :: Test
testExtractRoot = TestCase $
    assertEqual "Extract root"
        (JsonObject [("a", JsonNumber 1)])
        (extractPath (JsonObject [("a", JsonNumber 1)]) "$")

testExtractSimple :: Test
testExtractSimple = TestCase $
    assertEqual "Extract simple path"
        (JsonNumber 1)
        (extractPath 
            (JsonObject [("a", JsonNumber 1)]) 
            "$.a")

testExtractNested :: Test
testExtractNested = TestCase $
    assertEqual "Extract nested path"
        (JsonNumber 1)
        (extractPath 
            (JsonObject [("a", JsonObject [("b", JsonNumber 1)])]) 
            "$.a.b")

testExtractArray :: Test
testExtractArray = TestCase $
    assertEqual "Extract array element"
        (JsonNumber 1)
        (extractPath 
            (JsonObject [("arr", JsonArray [JsonNumber 1, JsonNumber 2])]) 
            "$.arr[0]")

testPrettyEmpty :: Test
testPrettyEmpty = TestCase $
    assertEqual "Pretty print empty object"
        "{\n}"
        (prettyStr (JsonObject []))

testPrettySimple :: Test
testPrettySimple = TestCase $
    assertEqual "Pretty print simple object"
        "{\n    \"name\": \"John\"\n}"
        (prettyStr (JsonObject [("name", JsonString "John")]))

testPrettyNested :: Test
testPrettyNested = TestCase $
    assertEqual "Pretty print nested object"
        "{\n    \"person\": {\n        \"name\": \"John\"\n    }\n}"
        (prettyStr (JsonObject [
            ("person", JsonObject [("name", JsonString "John")])
        ]))

-- Main test suite
tests :: Test
tests = TestList [
    TestLabel "Basic Tests" $ TestList [
        testEmptyObject,
        testSimpleObject,
        testMultipleTypes,
        testNestedObject,
        testObjectWithArray,
        testObjectInArray,
        testTrimming,
        testNullObject
    ],
    TestLabel "Sort Tests" $ TestList [
        testSort,
        testSortSimple,
        testSortNested
    ],
    TestLabel "Merge Tests" $ TestList [
        testMergeEmpty,
        testMergeSimple,
        testMergeArrays,
        testMergeConflict
    ],
    TestLabel "Minify Tests" $ TestList [
        testMinifyEmpty,
        testMinifySimple,
        testMinifyComplex
    ],
    TestLabel "Extract Tests" $ TestList [
        testExtractRoot,
        testExtractSimple,
        testExtractNested,
        testExtractArray
    ],
    TestLabel "Pretty Print Tests" $ TestList [
        testPrettyEmpty,
        testPrettySimple,
        testPrettyNested
    ]
    ]

