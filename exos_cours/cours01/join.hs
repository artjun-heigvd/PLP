join _ [] = []
join sep (x:xs)
    | null xs = x
    | otherwise = x ++ sep ++ join sep xs

-- Possible aussi
join' _ [] = []
join' _ [x] = x
join' sep (x:xs)= x ++ sep ++ join sep xs
