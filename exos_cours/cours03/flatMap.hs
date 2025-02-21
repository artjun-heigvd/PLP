import Data.List
-- Write function that maps a list producing a list of lists and flattens it
-- ex: flatMap singleton [1..5] => [1, 2, 3, 4, 5]
-- (map singleton [1..5] => [[1], [2], [3], [4], [5]])

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap _ []  = []
flatMap f (x:xs) = f x ++ flatMap f xs