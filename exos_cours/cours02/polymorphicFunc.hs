--Write function that verify if sorted list
-- ex : sorted [4, 2, 3, 1]
-- give False
-- must be Generic, etc.
-- Following template : sorted xs = ??

sorted :: Ord a => [a] -> Bool
                        

sorted (x:xs)           | null xs = True
                        | x > head xs = sorted xs
                        | otherwise = False