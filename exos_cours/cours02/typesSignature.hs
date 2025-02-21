f :: (Enum a, Eq a, Num a) => a -> a
f x = if x == 1 then x else x * f (pred x)

g :: (Enum a, Eq a) => a -> ([a], [a], Bool)
g x = (z, z, z /= z) where z = [x..]

h :: Show a => a -> String -> String -> [String]
h x y z = show x : y : z : h x y z
-- Car quand on String : String ca retourne [String]

i :: (Ord a, Fractional a) => a -> a
i x | x > 0 = 1 / x

j :: Ord a => [a] -> Bool
j x = let (a:b) = x; (i:j) = b; (u:_) = j in a > i && a < u