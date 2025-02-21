import System.Environment
-- Authors : Junod Arthur, Häffner Edwin
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Run-Length Encoding Compression
rlec :: String -> String
rlec "" = ""
rlec str = compressRLE str

compressRLE :: String -> String
compressRLE (x:xs) = countOccurrences x xs 1 ""
    where
    countOccurrences :: Char -> String -> Int -> String -> String
    countOccurrences toCount [] count acc = reverse (toCount : intToChar count : acc)
    countOccurrences toCount (x:xs) count acc
        | isDigit x = error "Error: Can't have a number to compress in the input !"
        | toCount == x = countOccurrences toCount xs (count + 1) acc
        | otherwise = countOccurrences x xs 1 (toCount : intToChar count : acc)


-- Run-Length Encoding Decompression
rled :: String -> String
rled "" = ""
rled str = decompressRLE str

decompressRLE :: String -> String
decompressRLE str = unpackOccurences str ""
    where
      unpackOccurences :: String -> String -> String
      unpackOccurences [] acc = acc
      unpackOccurences str acc =
        let (count, rest) = getNumber str 0
        in case rest of
          [] -> error "Error: Can't end input with a number !"
          (x:xs) -> unpackOccurences xs (acc ++ unpackOnePacket count x)

intToChar :: Int -> Char 
intToChar i = toEnum (fromEnum '0' + i)

getNumber :: String -> Int -> (Int, String)
getNumber [] n = (n, [])
getNumber (x:xs) acc 
-- The "*10" on the acc allow us to read the number even if it has multiple digits
    | isDigit x = getNumber xs (acc * 10 + read [x]) 
    | otherwise = (acc, x:xs)

-- Unpacks a packet of data like "5A" into it's uncompressed value "AAAAA"
unpackOnePacket :: Int -> Char -> String
unpackOnePacket nb value = unpackOnePacketAcc nb value ""
    where
        unpackOnePacketAcc :: Int -> Char -> String -> String
        unpackOnePacketAcc 0 _ acc = acc
        unpackOnePacketAcc nb value acc = unpackOnePacketAcc (nb - 1) value (value : acc)

-- Compress a file using RLE
compress :: FilePath -> FilePath -> IO ()
compress inputPath outputPath = do
  content <- readFile inputPath
  let compressed = rlec content
  writeFile outputPath compressed

-- Decompress a file using RLE
decompress :: FilePath -> FilePath -> IO ()
decompress inputPath outputPath = do
  content <- readFile inputPath
  let decompressed = rled content
  writeFile outputPath decompressed

-- Usage function to print help message
usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ "Usage: " ++ prog ++ " [<option>] <input-file> <output-file>"
  putStrLn "Options:"
  putStrLn "  -c  Compress the input file"
  putStrLn "  -d  Decompress the input file"

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-c", inputFile, outputFile] ->
      compress inputFile outputFile
    ["-d", inputFile, outputFile] ->
      decompress inputFile outputFile
    _ -> usage
