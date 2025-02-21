import System.Directory.Internal.Prelude (getArgs)
-- Implémenter un programme "head" qui permet de retourner les n premières 
-- lignes ou les c premiers bytes d'un ou de plusieurs fichiers donnés
-- Si plusieurs fichier sont données il faut les séparer par ==> XXX <== avec XXX le nom du fichier

findNb :: IO [String] -> (String, Int)
findNb (argsh:argst)
    | argsh == "-n" = ("line", head argst)
    | argsh == "-c" = ("char", head argst)
    | otherwise = ("void", 10)

openFiles :: IO [String] -> [IO String]
openFiles args = [list : readFile file | file <- files]
    where files = drop 2 args

main :: IO ()
main = do
    args <- getArgs
    (typ, nb) <- findNb args
    files <- openFiles args
    if typ == "line" then
        -- aa
    else if typ == "char" then
        
    else

