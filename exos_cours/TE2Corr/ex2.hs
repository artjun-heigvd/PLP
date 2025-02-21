a = fmap (fmap (fmap (const (Just Nothing)))) (Just ( Just Nothing))
-- 1er fmap
-- dans f on met (fmap (fmap (const (Just Nothing))))
-- fmap f (Just (Just Nothing))
-- Just (f (Just Nothing))
-- > Just (fmap (fmap (const (Just Nothing))) (Just Nothing)

-- 2ème fmap
-- dans g on met (fmap (const (Just Nothing)))
-- Just (fmap g (Just Nothing))
-- Just (Just (g Nothing))
-- > Just (Just (fmap (const (Just Nothing)) Nothing))

-- 3ème fmap
-- Just (Just (fmap (const (Just Nothing)) Nothing))
-- Ici le const (Just Nothing) ne sera jamais évalué pour le 3ème fmap car :
-- instance Functor Maybe where
--    fmap f Nothing = Nothing : On évalue pas f
--    fmap f (just x) = f x
-- > Just (Just Nothing)

-- Réponse : Just (Just Nothing)

b = [(*2), (*3)] <*> [4, 5] <$ Nothing <$ Just "Hello"
 -- [(*2), (*3)] <*> [4, 5]
 -- on applique *2 a tous les éléments de [4, 5] et pareil pour *3
 -- > [8, 10, 12, 15]

 -- [8, 10 ,12, 15] <$ Nothing
 -- pareillement que fmap : (<$) a Nothing = Nothing
 -- > Nothing 

-- Nothing <$ Just "Hello"
-- (<$) a (Just b) = Just a
-- > Just Nothing

-- Réponse : Just Nothing

c = pure (++) <*> ["a", "b"] <*> ["c", "d"] >>= (\x -> [x ++ "e"]) >>= (\y -> [y ++ "f"])
-- pure (++) <*> ["a", "b"]
-- On insère la fonction (++) dans le contexte de la list en l'appliquant à ses éléments
-- > ["a" ++, "b" ++]

-- ["a" ++, "b" ++] <*> ["c", "d"]
-- > ["ac", "ad", "bc", "bd"]

-- ["ac", "ad", "bc", "bd"] >>= (\x -> [x ++ "e"])
-- >>= prend chaque élément de la liste pour lui appliquer le lambda donné
-- > ["ace", "ade", "bce", "bde"]

-- ["ace", "ade", "bce", "bde"] >>= (\y -> [y ++ "f"])
-- > ["acef", "adef", "bcef", "bdef"]

-- Réponse : ["acef", "adef", "bcef", "bdef"]

d = (\(x:_) -> Just $ repeat x) [1..] >>= (\x -> Just $ drop 5 $ take 3 x)
-- (\(x:_) -> Just $ repeat x) [1..]
-- On répète le premier élément de la liste
-- > Just [1, 1, 1, 1, ...]

-- Just [1, 1, 1, 1, ...] >>= (\x -> Just $ drop 5 $ take 3 x)
-- drop 5 $ take 3 x retourne une liste vide
-- > Just []

-- Réponse : Just []

e = Just "hello" >>= (\x -> Just (x ++ " world")) >>= Just . reverse >>= (\z -> Just (z ++ "!"))
-- Just "hello" >>= (\x -> Just (x ++ " world"))
-- On prend l'élément dans le Maybe et on lui applique le lambda
-- > Just "hello world"

-- Just "hello world" >>= Just . reverse
-- Pareil qu'avant mais avec un appel de fonction (sans le . on met un lambda)
-- > Just "dlrow olleh"

-- Just "dlrow olleh" >>= (\z -> Just (z ++ "!"))
-- > Just "dlrow olleh!"

-- Réponse : Just "dlrow olleh!"


