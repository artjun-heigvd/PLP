-- Sol
unique xs = uniqueAcc xs []
    where 
        uniqueAcc [] acc = acc
        uniqueAcc (x:xs) acc
            | x `elem` acc = uniqueAcc xs acc
            | otherwise = uniqueAcc xs (x : acc)