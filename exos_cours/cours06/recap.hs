-- 1. Functors
{-Map over a context-}

{-
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
        (<$) :: a -> f b -> f a

        and to compare
        map :: (a -> b) -> [a] -> [b]
-}

e1 :: [Integer]
e1 = fmap (+1) [1, 2, 3]
-- => [2, 3, 4]

e2 :: Maybe Integer
e2 = fmap (+1) (Just 1)
-- => Just 2

e3 = fmap (+1) Nothing
-- => Nothing

e4 = 1 <$ Just 2
-- => Just 1

e5 = 1 <$ Nothing
-- => Nothing

e6 :: [Integer]
e6 = 1 <$ [1, 2, 3]
-- => [1, 1, 1]

e7 = 1 <$ []
-- => []

-- 2. Applicatives
{-Generalizing functors-}

{-
    class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
-}

-- instance Applicative Maybe where
--      pure :: a -> Maybe a
--      pure x = Just x

-- pure :: a -> Maybe a
-- ghci> pure 42 :: Maybe Int
-- => Just 42

{-
    Introduit une fonction dans le contexte donné, Maybe ici
    Pas besoin de gérer els erreurs, elles sont gérés par le contexte
-}

e8 = pure (+1) <*> Just 1
-- => Just 2

e9 = pure (+) <*> Just 1 <*> Just 2
-- > Just 3

e10 = pure (+) <*> Just 1 <*> Nothing
-- => Nothing

e11 = pure (+1) <*> [1, 2]
-- => [2, 3]

e12 = pure (+) <*> [1, 2] <*> [3, 4]
-- => [4, 5, 5, 6]

-- 3. Monads
{-Sequencing computations-}

{-
    class Applicative m => Monad m where
        return :: a -> m a
        (>>=) :: m a -> (a -> m b) -> m b

    ex avec IO
        (>>=) :: IO a -> (a -> IO b) -> IO b
        getLine >>= putStrLn >>= getLine >>= ...
-}


e13 = Just 1 >>= \x -> Just (x + 1)
-- => Just 2

e13' :: Maybe Int -> Maybe Int
e13' m = m >>= \x -> Just (x + 1)
-- => Just 2

e13'' = do
    x <- Just 1
    Just (x + 1)

e13''' m = do
    x <- m
    Just (x + 1)
-- Sucre sintaxique pour e13'

{-
    instance Monad Maybe where
        return :: a -> Maybe a
        return x = Just x

        (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
        (>>=) m f =
            case m of
                Nothing -> Nothing
                Just x -> f x
-}

e14 = Just 1 >>= \x -> Nothing
-- => Nothing

e14' = do
    x <- Just 1
    Nothing
-- => Nothing

e15 = [1, 2] >>= \x -> [x, x + 1]
-- => [1, 2, 2, 3]

e15' = do
    x <- [1, 2]
    [x, x + 1]
-- => [1, 2, 2, 3]

e16 = Just 1 >>= \x -> Just 2 >>= \y -> Just (x + y)
-- => Just 3

-- 4. Lazy evaluation
{-Delaying computation-}

{-
    N'évalue pas les variable ou fonctions avant qu'elles soient utiles ou utilisées
-}

e18 = take 5 [1..]
-- => [1, 2, 3, 4, 5]

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

repeat' :: t -> [t]
repeat' x = x : repeat' x

e19 = take 3 (repeat' 1)
-- => [1, 1, 1]

-- 5. Lambda calculus
{-Smallest functional programming language-}

{-
    1. Variables (x, y, z, ...)
    2. Abstraction (λx. ...)
    3. Application (f x)
-}

{-
    (λx y. x y) (λz. z) (λw. w)
    => (λy. (λz. z) y) (λw. w)
    => (λz. z) (λw. w)
    => (λw. w)
-}