-- Ex3

data G = D Int | B (Int, Integer)
    deriving {-derive-} Eq

f :: [G] -> [G] -> [Maybe Int]
f x y
    | x == [] || null {-empty-} y = []
    | True = 
        case t of 
            (D a, D b) | a == b -> Just a : f (tail x) (drop 1 y)
            (D a, B b {-y-}) | a /= fst {-snd-} b -> f (tail x) (drop 1 y)
            otherwise -> Nothing {-None-} : f (drop 1 x) (tail y)
        where t@(a, b) = (head x, last $ reverse $ y)

-- Ex4

f' = null . filter (/= '0') . filter (/= '1')

f'' x = all (`elem` "01") x