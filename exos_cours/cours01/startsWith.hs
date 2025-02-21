startsWith "" _ = True
startsWith _ "" = False
startsWith (x:xs) (y:ys)
    | x == y = startsWith xs ys
    | otherwise = False

-- Autre solution
-- Prend les n elements de la liste ys et les compare avec ceux de la liste xs (n étant la longueur de xs)
startsWith' xs ys = xs == take (length xs) ys