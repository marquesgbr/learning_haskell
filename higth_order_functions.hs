sumFun :: (Integer - > Integer) -> Integer -> Integer
sumFun f 0 = f 0
sumFun f n = (f n) + (sumFun f (n - 1))

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p xs = [x | x <- xs, p x] 

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = []
filter2 p (x:xs)
 | p x       = x : filter2 p xs
 | otherwise = filter2 p xs 

zipWith' :: (a -> b -> c)  [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' xs ys
zipWith' f _ _           = []

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x]    = x
foldr1 f (x:xs) = f x (foldr1 f xs) 

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s []     = s
foldr f s (x:xs) = f x (foldr f s xs)

foldl :: (a -> b -> b) -> b -> [a] -> b
foldl f s []     = s
foldl f s (x:xs) = foldl f (f s x) xs

foldr1 f xs = foldr f (last xs) (init xs)

{-
foldl (+) 0 [1,2,3]
= foldl (+) ((+) 0 1) [2,3]
= foldl (+) (1) [2,3]
= foldl (+) ((+) 1 2) [3]
= foldl (+) (3) [3]
= foldl (+) ((+) 3 3) []
= foldl (+) 6 []
= 6
-}

concat :: [[a]] -> [a]
concat xxs = foldr (++) [] xxs

and :: [Bool] -> Bool
and xs = foldr (&&) True xs

rev :: [a]-> [a]
rev xs = foldr snoc [] xs
snoc :: a-> [a]-> [a]
snoc x xs = xs ++ [x]

zipWith ($) [sum,product] [[1,2],[3,4]]

addNum n m = n + m

addNum' n = (\m -> n + m)

-- g (f x) (f y)

plumbing :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
plumbing f g = (\x y -> g (f x) (f y))

{- Exemplo do LHFGG: Tem a ver com curring
let listOfFuns = map (*) [0..]
(listOfFuns !! 4) 5
20
-}

iter :: Int -> ( t −> t ) −> ( t −> t )
iter 0 f = id
iter n f = (iter (n−1) f ) . f
