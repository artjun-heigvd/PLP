-- fst ((even 128, id "foo"), ())       :: (Bool, String)

-- 3.1415926535869793                   :: Fractional 

-- head (head [show 42, "42"])          :: Char   

-- length ['z'..] `div` 2               :: Int
-- Explication: --
-- [z..]    :: [char]
-- 2        :: Num a => a
-- length   :: [a] -> Int
-- div      :: Integral

-- head (singleton True) : [not False]  :: [Bool] 