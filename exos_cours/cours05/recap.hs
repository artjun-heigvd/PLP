import GHC.Base (VecElem(Int16ElemRep))
import Control.Monad (when, unless)
import System.Directory.Internal.Prelude (getArgs)

-- 1. IO (input / output)

-- return   :: a -> IO a
createTuple :: a -> b -> IO (a, b)
createTuple x y = return (x , y)

-- (>>=)    :: IO a -> (a -> IO b) -> IO b
echo :: IO ()
echo = putStrLn "Give me a value" >> getLine 
    >>= \str -> putStrLn ("You gave me: " ++ str)

-- Si que des >>= sont utilisés mets en évidence la différence entre >> et >>=
-- >> va ingnorer dans tous les cas la valeur a tandis que >>= attend qu'on la traite (avec éventuellement un callback vide si on veut l'ignorer)
echo' :: IO ()
echo' = putStrLn "Give me a value" >>= \_ -> getLine
    >>= \str ->  putStrLn ("You gave me: " ++ str)

-- (>>)     :: IO a -> IO b -> IO b
-- getChar  :: IO Char
-- getLine  :: IO String
-- putChar  :: Char -> IO ()
-- putStr   :: String -> IO ()
-- putStrLn :: String -> IO ()
-- print    :: Show a => a -> IO ()

-- 2. do block

-- do
echo'' :: IO ()
echo'' = do
    putStrLn "Give me value"
    str <- getLine
    putStrLn ("You gave me: " ++ str)

-- let
truc :: IO ()
truc = do
    let 
        machin = 42
        bidule = 12
    (a, b) <- createTuple True "foo"
    print machin

-- 3. control flow actions

-- mapM_    :: (a -> IO b) -> [a] -> IO ()
-- forM_    :: [a] -> (a -> IO b) -> IO()
-- when     :: Bool -> IO () -> IO ()
-- unless   :: Bool -> IO () -> IO ()

printIfEven :: IO ()
printIfEven = do
    str <- getLine
    let n = read n :: Int
    when (even n) (print n)
    unless (not (even n)) (print n)
    -- Equivalent:
    -- if even n then do print n
    -- else return ()
    putStrLn "done"

-- 4. Modules

-- prelude
-- module
-- import
-- hiding
-- alias
-- export

-- Name.hs
-- module Name (pi, foobar) where
data T = U Int
pi = 3.14
foobar x y = x + y

-- Main.hs
-- import Name (foobar)
-- import Name as N
-- Cache une définition pour ne pas avoir de conflit
-- import Name hiding (T)

-- 5. programs

-- main
main :: IO ()
main = do
    args <- getArgs
    if null  args then
        print "usage: truc bidule"
    else
        print args
-- ghc
-- getArgs
-- getProgName

-- 6. files

-- readFile     :: FilePath -> IO String
-- writeFile    :: FilePath -> String -> IO ()
-- lines        :: String -> [String]