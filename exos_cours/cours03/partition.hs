-- Write a function that returns the pair of lists of elements which do and do not satisfy a predicate
-- ex: partition (<3) [1, 2, 3, 4, 1, 2, 3, 4] => ([1, 2, 1, 2], [3, 4, 3, 4])

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' p xs = partitionAcc xs ([], [])
    where 
        partitionAcc [] (ys, zs) = (reverse ys, reverse zs)
        partitionAcc (x:xs) (ys, zs)
            | p x = partitionAcc xs (x:ys, zs)
            | otherwise = partitionAcc xs (ys, x:zs)

partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' _ [] = ([], [])
partition'' p (x:xs)
    | p x = (x:ys, zs)
    | otherwise = (ys, x:zs)
    where (ys, zs) = partition'' p xs

-- Execution de partition''
-- partition'' even [1, 2, 3]
--      | even 1 =
--      | otherwise = (ys, 1:zs) <=> ([2], [1, 3])
-- => partition'' even [2, 3]
--      | even 2 = (2:ys, zs) <=> ([2], [3])
--      | otherwise = 
-- => partition'' even [3]
--      | even 3 =
--      | otherwise = (ys, 3:zs) <=> ([], [3])
-- => (ys, zs) = ([], []) = partition'' even []