data Lambda 
    = Var String        -- x
    | App Lambda Lambda -- x y
    | Abs String Lambda -- λx. x

-- (λx. x) z
-- => z
-- trad en Data :
-- App (Abs "x" (Var "x")) (Var "z")

betaReduce :: Lambda -> Lambda
betaReduce (Var name) = Var name
betaReduce (Abs name body) = Abs name (betaReduce body) -- Il acceptait aussi Abs name body sans l'appel à betaReduce
betaReduce (App (Abs name body) arg) = betaReduce (subst name body arg) -- L'appel à betaReduce pas obligatoire
betaReduce _ = error "unexpected"

-- A tester pas sur (exemple de subst)
subst :: String -> Lambda -> Lambda -> Lambda
subst x n (Var y) = if x == y then n else Var y
subst x n (App m1 m2) = App (subst x n m1) (subst x n m2)
subst x n (Abs y m) = if x == y then Abs y m else Abs y (subst x n m)
