-- Write a non-recursive sum that return the sum of squared even numbers of a list of integers
-- ex: sum [1, 2, 3, 4, 5] => 20

sumNR :: [Int] -> Int
sumNR = foldl (\acc x -> if even x then acc + (x*x) else acc) 0

sumNR' :: [Int] -> Int
sumNR' = foldl sumSquareEven 0

sumSquareEven acc x
    | even x = acc + x*x
    | otherwise = acc

sumNR'' :: [Int] -> Int
sumNR'' = sum . map (^2) . filter even