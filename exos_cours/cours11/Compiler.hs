import Pipi
import Codegen

main = do
    let sourceCode = [Print (Add (Number 1) (Number 2))]
    let executableCode = codegen sourceCode
    let executableText = unlines $ map show executableCode
    writeFile "a.out" executableText