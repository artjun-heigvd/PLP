import Data.Char
{-
Write a tokenizer that tokenizes λ-calculus terms
    tokenize "(λx.x)z"
should give
    [LPar, Lambda, Var 'x', Dot, Var 'x', RPar, Var 'z']
-}

data Token = LPar | RPar | Lambda | Dot | Var Char
    deriving(Show)

tokenize :: String -> [Token]
tokenize str = tokenizeAcc str []
    where tokenizeAcc str acc = case str of
            ('(':rest) -> tokenizeAcc rest (LPar:acc)
            ('λ':rest) -> tokenizeAcc rest (Lambda:acc)
            ('.':rest) -> tokenizeAcc rest (Dot:acc)
            (')':rest) -> tokenizeAcc rest (RPar:acc)
            (c:rest)
                | isSpace c -> tokenizeAcc rest acc
                | isAlpha c -> tokenizeAcc rest (Var c:acc)
                | otherwise -> error ("unexpected character: " ++ [c])
            "" -> reverse acc