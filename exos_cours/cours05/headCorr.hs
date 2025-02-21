import System.Environment ( getArgs, getProgName )
import Control.Monad ( unless, forM_ )
data Option = Bytes Int | Count Int
    deriving Show

-- Le $ sert à éviter d'utiliser des parenthèses: f (x y) = f $ x y
-- take n [] prend les n premier de []
-- case of permet de faire du pattern matching sur plusieurs cas
-- Utiliser do quand il a une interaction avec l'IO
-- unless ou when quand il y a des cas à gérer

parseArgs :: [String] -> (Option, [String])
parseArgs args = 
    case args of
        ("-c":n:rest) -> (Bytes $ read n, rest)
        ("-n":n:rest) -> (Count $ read n, rest)
        _ -> (Count 10, args)

bytes :: Int -> String -> String
bytes n str
    | n <= 0 = error ("head: illegal byte count -- " ++ show n)
    | otherwise = take n str

count :: Int -> String -> String
count n str
    | n <= 0 = error ("head: illegal line count -- " ++ show n)
    | otherwise = unlines $ take n $ lines str

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn (progName ++ "[-n count | -c bytes] [files...]")
    putStrLn "  -c bytes - Print bytes of each of the specified files."
    putStrLn "  -n count - Print count lines of each of the specified files." 

main :: IO ()
main = do
    args <- getArgs
    if null args then usage
    else do
        let (option, files) = parseArgs args
        let action = case option of 
                Count n -> count n
                Bytes n -> bytes n
        unless (null files) $ do
            if length files == 1 then
                processFile action (head files)
            else
                forM_ files $ \file -> do
                    putStrLn ("==> " ++ file ++ " <==")
                    processFile action file
        where
            processFile action file = do
                contents <- readFile file
                putStrLn $ action contents
