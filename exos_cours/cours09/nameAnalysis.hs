{-
Given the following language
-}

data Expr                   -- exs:
    = Cst Int               -- 42
    | Var String            -- "x"
    | Bin Expr Char Expr    -- 1 + "foo"
    | Let String Expr Expr  -- let z = 0 in z + 1
    deriving Show

{-
Write a function check that returns all the declarations
 that shadow a variable declared in an outer scope.
 
 ex: let z = 0 in
        let y = 1 in
            let z = 2 in
                y + z
-}

check :: Expr -> [(String, Expr)]
check expr = checkMem expr []
    where
        checkMem :: Expr -> [String] -> [(String, Expr)]
        checkMem (Bin left _ right) mem = checkMem left mem ++ checkMem right mem
        checkMem (Let name val expr) mem = 
            if name `elem` mem 
                then (name, val) : (checked ++ checkMem expr mem)
                else checked ++ checkMem expr (name : mem)
            where
                checked = checkMem val mem -- On regarde s'il y a du shadowing dans la valeur donnée au let
        checkMem _ _ = []