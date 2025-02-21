import System.Environment

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "usage : " ++ prog ++ " <input> <output>"

ln :: [String] -> [String]
ln xs = zipWith (\x n -> show n ++ " " ++ x ) xs [1..]
-- Variable sans zipWith (Attention! le lambda prend un tuple et non 2 paramètres) :
-- ln xs = map (\(x, n) -> show n ++ " " ++ x ) $ zip xs [1..]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then usage
    else do
        let [input, output] = args
        content <- readFile input
        let 
            lines' = ln (lines content)
            newContent = unlines lines'
        writeFile output newContent