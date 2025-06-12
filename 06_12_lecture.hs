
-- Verifica se um elemento pertence a uma lista
member :: Eq t => [t] -> t -> Bool
member [] _ = False
member (x:xs) y 
    | x == y = True
    | otherwise = member xs y


data ListaInt = Vazia 
                | Cons Int ListaInt


data Tree t = Nil t 
            | Node t (Tree t) (Tree t)
            deriving (Eq, Show, Ord)

instance Show ListaInt where
    show Vazia = "[]"
    show (Cons x l) = show x ++ " , " ++ show l

data Lista t = Nula 
                | Const t (Lista t)

instance (Show t) => Show (Lista t) where
    show Nula = "[]"
    show (Const x l) = show x ++ ", " ++ show l

class Visivel t where
    toString :: t -> String
    size :: t -> Int
    size _ = 1 -- tamanho padrao para tipos que nao implementam size

instance Visivel Bool where
    toString True = "True"
    toString False = "False"

instance Visivel Int where
    toString x = show x
    size _ = 120 -- tamanho padrao para Int (arbitrário)

instance (Visivel t) => Visivel [t] where
    toString xs = "[" ++ (concatMap toString xs) ++ "]"
    size xs = (foldr (+) 0) (map size xs)

iSort :: Ord t => [t] -> [t]
iSort [] = []
iSort (x:xs) = iSort [y | y <- xs, y <= x] ++ [x] ++ iSort [y | y <- xs, y > x]

class (Ord t, Visivel t) => OrdVis t

vSort :: OrdVis t => [t] -> String
vSort = toString . iSort

{-
Main> vSort [3::Int, 1::Int, 4::Int, 1::Int, 5::Int, 9::Int, 2::Int, 6::Int]
"1, 1, 2, 3, 4, 5, 6, 9"
-}


--TODO make this compile 
main = do l <- getLine
        do l2 <- getLine
        putStrLn(l ++ "     " ++ l2)
        putStrLn (show (member [1,2,3,4] 3))
        putStrLn(l2)
        do l3 <- return "sgfdfadf" -- diferente do return de linguagem imperativa
        putStrLn(l3)