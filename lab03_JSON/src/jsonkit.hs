{-
    Main funtion for the use of our programm

    Authors: Edwin Häffner, Arthur Junod
-}

import JsonType
import PrettyPrintJson
import MinifyJson
import MergeJson
import ParseJson
import ExtractJsonPath
import System.Environment
import Control.Exception
import System.IO.Unsafe
import Debug.Trace
import Data.Char (isDigit)
import SortJson (sortJsonKeys)
import Test.HUnit
import JsonTest

-- Function that shows the usage of the program
usage :: IO()
usage = do
    putStrLn "% ./jsonkit"
    putStrLn  "Usage: jsonkit <operation>"
    putStrLn  "Operations:"
    putStrLn  "  -pretty  <file>                     | Format a JSON file in a human-readable way."
    putStrLn  "  -minify  <file>                     | Remove all unnecessary whitespace from a JSON file."
    putStrLn  "  -extract <file> <path>              | Extract a specific value from a JSON file using a path expression."
    putStrLn  "  -merge   <file> <file>...           | Merge two or more JSON files into one."
    putStrLn  "  -sort    <file>                     | Sort the keys in a JSON object alphabetically.print"
    putStrLn  "  -test                               | Run the test suite."

-- Helper function to merge multiple JSON files
mergeFiles :: [FilePath] -> IO JsonVal
mergeFiles [] = return JsonNull
mergeFiles [file] = processJsonFile file
mergeFiles (file:files) = do
    content <- processJsonFile file
    rest <- mergeFiles files
    return $ mergeJson content rest

-- Helper function to process a JSON file
processJsonFile :: FilePath -> IO JsonVal
processJsonFile file = do
    content <- readFile file
    return $ parseJson (trimming content)

-- Helper function for operations that don't need an additional argument
handleJsonOperation :: (JsonVal -> JsonVal) -> FilePath -> IO ()
handleJsonOperation operation file = do
    parsedJson <- processJsonFile file
    putStrLn $ prettyStr $ operation parsedJson

-- Helper function for operations that need an additional argument
handleJsonOperationWithArg :: (JsonVal -> String -> JsonVal) -> FilePath -> String -> IO ()
handleJsonOperationWithArg operation file arg = do
    parsedJson <- processJsonFile file
    putStrLn $ prettyStr $ operation parsedJson arg

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-test"] -> runTestTT tests >> return ()
        (op:file:rest) -> case op of
            "-pretty"    -> handleJsonOperation id file
            "-minify"    -> processJsonFile file >>= putStrLn . minifyJson
            "-merge"     -> mergeFiles (file:rest) >>= putStrLn . prettyStr
            "-extract"   -> handleJsonOperationWithArg extractPath file (head rest)
            "-sort"      -> handleJsonOperation sortJsonKeys file
            _           -> usage
        _ -> usage