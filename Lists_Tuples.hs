ex1 = take 2 [1,2,3,4] -- [1,2]
ex2 = take 5 [1,2,3,4] -- [1,2,3,4]
ex3 = take 0 [1,2]     -- []

take' :: Integer -> [a] -> [a]
take' n _
 | n <= 0 = []
take' _ []     = []
take' n (x:xs) = x : (take' (n-1) xs) 

ex4 = drop 2 [1,2,3,4] -- [3,4]
ex5 = drop 5 [1,2,3,4] -- []
ex6 = drop 0 [1,2,3,4] -- [1,2,3,4]

drop' :: Integer -> [a] -> [a]
drop' n xs
 | n <= 0 = xs
drop' _ []     = []
drop' n (x:xs) = drop (n - 1) xs

splitAt' :: Integer -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' 0 xs = ([], xs)
splitAt' n xs = (take n xs, drop n xs)

ex7 = minimum []    -- Erro
ex8 = minimum [1,2] -- 1

ex9  = maximum []    -- Erro
ex10 = maximum [1,2] -- 2

min' :: (Ord a) => a -> a -> a
min' x y
 | x <= y    = x
 | otherwise = y

max' :: (Ord a) => a -> a -> a
max' x y
 | x >= y    = x
 | otherwise = y

minimum' :: (Ord a) => [a] -> a
minimum' []     = error "Não existe mínimo de lista vazia"
minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "Não existe máximo de lista vazia"
maximum [x] = x
maximum' (x:xs) = max' x (maximum' xs)

minimum2 :: (Ord a) => [a] -> a
minimum2 []  = error "Não existe mínimo de lista vazia"
minimum2 [x] = x
minimum2 (x:xs)
 | x < minimum2 xs = x
 | otherwise       = minimum2 xs

maximum2 :: (Ord a) => a -> a -> a
maximum2 []  = error "Não existe máximo de lista vazia"
maximum2 [x] = x
maximum2 (x:xs)
 | x > maximum2 xs = x
 |otherwise        = maximum2 xs

ex11 = replicate 3 5       -- [5,5,5]
ex12 = replicate 2 [1,2,3] -- [[1,2,3], [1,2,3]]

replicate' :: Integer -> a -> [a]
replicate' n _
 | n <=0 = []
replicate' n x = x : replicate' (n-1) x

ex13 = repeat 5      = [5,5,5, ...]
ex14 = cycle  [1,2]  = [1,2,1,2,1,2, ...]

repeat' :: a -> [a]
repeat' x = x:repeat' x

cycle' :: [a] -> [a]
cycle' [] = error "lista Vazia"
cycle' xs = xs ++ (cycle' xs)

cycle2 :: [a] -> [a]
cycle2 [] = error "lista Vazia"
cycle2 xs = let ys = xs ++ ys in ys 

ex15 = elem 5 [1,3,4,5] -- True
ex16 = elem 'a' [] -- False

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
 | x == y    = True
 | otherwise = elem' x ys


ex17 = zip [1,2] [3,4]      -- [(1,3), (2,4)]
ex18 = unzip [(1,3), (2,4)] -- ([1,2], [3,4])

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ _ = []

-- REVER ABAIXO

unzip' :: [(a,b)] -> ([a], [b])
unzip' []     = ([],[])
unzip' ((a,b):xs) = (a: restanteEsquerda, b: restanteDireita)
        where (restanteEsquerda, restanteDireita) = unzip' xs

ex19 = reverse "ABCD" -- "DCBA"

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = (reverse' xs) ++ [x] 

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length(xs)

length2 :: [a] -> Integer
length2 [] = 0
length2 xs = sum [1 | _ <- xs]

ex20 = concat [[1,2], [3,4]] -- [1,2,3,4]

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ (concat' xs)

--REFAZER

(++) :: [a] -> [a] -> [a]
[] ++ [] = []
[] ++ ys = ys
(x:xs) ++ ys = x: (xs ++ ys)

remainder :: Integer -> Integer -> Integer
remainder _ 0 = error "divisão por zero"
remainder x y
 | x < y     = x
 | otherwise = remainder (x - y) y  

divisor :: Integer -> Integer -> Integer
divisor _ 0 = error "divisão por zero"
divisor x y
 | x < y     = 0
 | otherwise = 1 + divisor (x - y) y

-- takeWhile (/=' ') "elephants know how to party" == "elephants"

takeWhile' (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
 | p x = x: takeWhile' p xs
 | otherwise = []

dropWhile' (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f all@(x:xs)
 | f x       = dropWhile' f xs
 | otherwise = all

fib :: Integer -> Integer
fib 0 = 0
fiz 1 = 1
fib n = fib(n - 1) + fib(n - 2)
--REFAZER
fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (x, y) = (y, x + y)

fibPair :: Integer -> (Integer, Integer)
fibPair n
 | n == 0 = (0,1)
 | otherwise = fibStep(fibPair(n-1))

 -- otherwise = let (a, b) = fibPair(n - 1) in (b, a+b)
 -- otherwise = (y, x+y) where (x, y) = fibPair(n - 1)
 -- otherwise = \(n-1) -> (y, x+y) where (x,y) = fibPair(n - 1)
 -- otherwise = (\(x,y) -> (y, x+y)) (fibPair (n - 1)) 


fastfib :: Integer -> Integer
fastfib = fst . fibPair

onSeparateLines :: [String]->String -- Equivalente a unlines do Prelude
onSeparateLines []     =  []
onSeparateLines (x:xs) = x ++ "\n" ++ (onSeparateLines xs) 

fibTable::Integer->String
fibTable n = "n         fib n\n" ++ onSeparateLines [show i ++ "         " ++ show       (fastfib i) | i <- [0..n]]

-- Versão mais enxuta para fastfib:

fastfib2 :: Integer -> Integer
fastfib2 n = go n (0,1)
    where
        go k (a,b)
         | k == 0    = a
         | otherwise = go (k-1) (b, a+b)  

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = menores ++ [x] ++ maiores
    where
        menores = quickSort [y | y <- xs, y <= x]
        maiores = quickSort [z | z <- xs, z > x]

insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = ins x (insertSort xs)

ins :: (Ord a) => a -> [a] -> [a]
ins n [] = [n]
ins n (x:xs)
 | n <= x = n:x:xs
 | otherwise = x:ins n xs

-- [1,4,2,3]
-- [1,4]
-- [1]

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort(fst split xs)) (mergeSort(snd split xs))

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge all1@(x:xs) all2@(y:ys)
 | y <= x = y:merge all1 ys
 | otherwise = x:merge xs all2

split :: [a] -> ([a], [a])
split [] = ([],[])
split [x] = ([x],[])
split xs = splitAt ((length xs) `div` 2) xs


-------------------------------------------

BÔNUS: ALGUMAS FUNÇÕES DE Data.List

{-

nub -> elimina duplicatas

intersperse -> adiciona um elemente entre cada par de elementos

Ex: intersperse '.' "MONKEY" --> "M.O.N.K.E.Y"

takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] --> [6,5,4]

ghci> any (==4) [2,3,5,6,1,4]
True
ghci> all (>4) [6,9,10]
True
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
False
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
True

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

group agrupo elementos consecutivos iguais. Ex:

group :: Eq a => [a] -> [[a]]
ghci> group [1,1,2,3,3,3,4]
[[1,1],[2],[3,3,3],[4]]

ghci> intercalate " " ["hey","there","guys"]
"hey there guys"

transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]

concatMap (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]

ghci> any (==4) [2,3,5,6,1,4]
True
ghci> all (>4) [6,9,10]
True

take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]

ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> words "hey these           are    the words in this\nsentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> unwords ["hey","there","mate"]
"hey there mate"

delete takes an element and a list and deletes the first occurence of that element in the list.

ghci> delete 'h' "hey there ghang!"
"ey there ghang!"

\\ is the list difference function. It acts like a set difference, basically. For every element in the right-hand list, it removes a matching element in the left one.

ghci> [1..10] \\ [2,5,9]
[1,3,4,6,7,8,10]
ghci> "Im a big baby" \\ "big"
"Im a  baby"

[1,2,3,4,5,6,7,8,9,9,10] \\ [2,5,9]
[1,3,4,6,7,8,9,10]

ghci> "hey man" `union` "man what's up"
"hey manwt'sup"
ghci> [1..7] `union` [5..10]
[1,2,3,4,5,6,7,8,9,10]

-}

-- Retorna todas as sublistas de uma lista
-- Exemplo: sublistas [1,2,3] = [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sublistas xs ++ map (x:) (sublistas xs)
-- ou 
sublistas2 :: [a] -> [[a]]
sublistas2 [] = [[]]
sublistas2 (x:xs) = [x:ys | ys <- sublistas2] ++ sublistas2

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]:_) = []
transpose' matrix = firstColumn : transpose' rest
    where
        firstColumn = map head matrix
        rest = map tail matrix