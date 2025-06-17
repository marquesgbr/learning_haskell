{- Função que verifica se um elemento pertence a uma lista -}
member :: Eq t => [t] -> t -> Bool
member [] _ = False
member (x:xs) y 
    | x == y = True
    | otherwise = member xs y

-- Outra maneira
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = x == y || elem' y xs


