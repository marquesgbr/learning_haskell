type Person = String
type Book = String

type DataBase = [(Person, Book)]

exampleBase :: Database
exampleBase =[("Alice","Tintin") ,("Anna","LittleWomen"), ("Alice" , "Asterix") ,   ("Rory" , "Tintin")]

books :: DataBase -> Person -> [Book]

books db p = [book | (person, book) <- db,p == person]

persons :: DataBase -> Book -> [Person]

persons db book = [p | (p,b) <- db, b == book]

foiPego :: DataBase -> Book -> Bool

foiPego db book = not([] == [p | (p,b) <- db, book == b])

numPegos :: DataBase -> Person -> Integer

numPegos db person = sum [1 | (p,b) <- db,p == person]

inserir :: DataBase -> Person -> Book -> DataBase

inserir db person book = db ++ [(person, book)]

retirar :: DataBase -> Person -> Book -> DataBase

retirar db person book = [par | par <- db, par /= (person, book)]


-- TIPO ENUMERÁVEL (ENUMERATED ALGEBRIC TYPES):
-- CONSTRUTORES NÃO POSSUEM ARGUMENTOS

data Season = Summer | Winter | Autumn | Spring

-- TIPO PRODUTO (PRODUCT ALGEBRIC TYPES):
-- POSSUI ÚNICO CONSTRUTOR

type Name = String
type Age = String

data Person = Person Name Age

-- TIPE SUM (SUM ALGEBRIC TYPES):
-- MAIS GERAIS. CONSTUTORES PODEM POSSUEM VÁRIOS ARGUMENTOS

data Shape = Circle Float | Rectangle Float Float


data Expr = Lit Int
            | Add Expr Expr
            | Sub Expr Expr
                deriving(Show)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Sub exp1 exp2) = eval exp1 - eval exp2

show' :: Expr -> String
show' (Lit n) = show n
show' (Add exp1 exp2) = "(" ++ show' exp1  ++ "+" ++ show' exp2 ++ ")"
show' (Sub exp1 exp2) = "(" ++ show' exp1  ++ "-" ++ show' exp2 ++ ")"

-- Exemplo com constutores infixos:

data Expr = Lit Integer
          | Expr :+: Expr
          | Expr :-: Expr
          deriving (Show)

eval :: Expr -> Integer
eval (Lit n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :-: e2) = eval e1 - eval e2

numOpr :: Expr -> Int
numOpr (Lit n) = 0
numOpr (Add exp1 exp2) = 1 + numOpr exp1 + numOpr exp2
numOpr (Sub exp1 exp2) = 1 + numOpr exp1 + numOpr exp2

data Tree = Nil 
            | Node Integer Tree Tree
                deriving(Show, Eq, Ord)

sumTree, depthTree :: Tree -> Integer
sumTree Nil = 0
sumTree (Node n t1 21) = n + sum t1 + sum t2

depthTree Nil = 0
depthTree (Node _ t1 t2) = 1 + (max (depthTree t1) (depthTree t2))

occurs :: Tree -> Integer -> Integer
occurs Nil p = 0
occurs (Node n t1 t2)
 | n == p    = 1 + occurs t1 + occurs t2
 | otherwise = occurs t1 + occurs t2

leftHand :: Tree -> Tree
leftHand Nil = Nil
leftHand (Node _ t1 t2) = t1

rigthHand :: Tree -> Tree
rigthHand Nil = Nil
rigthHand (Node _ t1 t2) = t2

isElem :: Integer -> Tree -> Bool
isElem n (Nil) = False
isElem n (Node x t1 t2)
 | n == x    = True
 | otherwise = isElem n t1 || isElem n t2

maxTree :: Tree -> Integer
maxTree Nil = error "max de nó vazio"
maxTree (Node n t1 t2)
 | t1 == Nil && t2 == Nil = n
 | t1 == Nil = max n (maxTree t2)
 | t2 == Nil = max n (maxTree t1)
 | otherwise = max n ( max (maxTree t2) (maxTree t1))

minTree :: Tree -> Integer
minTree Nil = error "min de nó vazio"
minTree (Node n t1 t2)
 | t1 == Nil && t2 == Nil = n
 | t1 == Nil = min n (minTree t2)
 | t2 == Nil = min n (minTree t1)
 | otherwise = min n ( min (minTree t2) (minTree t1))

-- Sugestões do ChatGPT para max e min para evitar ter que derivar Ord:

maxTree :: Tree -> Integer
maxTree Nil = error "Árvore vazia"
maxTree (Node n t1 t2) = maximum [n, maxTreeOrZero t1, maxTreeOrZero t2]
  where
    maxTreeOrZero Nil = n -- ou Integer mínimo: minBound :: Integer
    maxTreeOrZero t = maxTree t

minTree :: Tree -> Integer
minTree Nil = error "Árvore vazia"
minTree (Node n t1 t2) = minimum [n, minTreeOrZero t1, minTreeOrZero t2]
  where
    minTreeOrZero Nil = n -- ou Integer máximo: maxBound :: Integer
    minTreeOrZero t = minTree t

reflect :: Tree -> Tree
reflect Nil = Nil
reflect (Node n t1 t2) = Node n (reflect t2) (reflect t1)

em_ordem :: Tree -> [Integer]
em_ordem Nil = []
em_ordem (Node n t1 t2) = em_ordem t1 ++ [n] ++ em_ordem t2

pre_ordem :: Tree -> [Integer]
pre_ordem Nil = []
pre_ordem (Node n t1 t2) = [n] ++ pre_ordem t1 ++ pre_ordem t2

pos_ordem :: Tree -> [Integer]
pos_ordem Nil = []
pos_ordem (Node n t1 t2) = pos_ordem t1 ++ pos_ordem t2 ++ [n]

-- Definição mais geral de Árvore:

data Tree a = Nil | Node a (Tree a) (Tree a)
                    deriving (Eq,Ord,Show,Read)

-- Posso fazer, por exemplo: Muitas funções ficam iguais, como as de percorrer

isElem :: (Eq a) => a -> Tree a -> Bool
isElem n (Nil) = False
isElem n (Node x t1 t2)
 | n == x    = True
 | otherwise = isElem n t1 || isElem n t2
 
-- print (isElem 3 (Node 3 (Node 4 Nil Nil) Nil))  --> True
-- print (isElem "Hello" (Node "Hello" (Node "World" Nil Nil) Nil)) --> True
-- print (isElem 'A' (Node '9' (Node 'a' Nil Nil) Nil)) --> False

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node n t1 t2) = Node (f n) (mapTree f t1) (mapTree f t2)

-- árvore binária

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x Nil = Node x Nil Nil
insertTree x (Node v left right)
    | x < v     = Node v (insertTree x left) right
    | otherwise = Node v left (insertTree x right)

-- Isto acaba colocando o último elemento da lista como a raíz
fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insertTree Nil

-- Isto acaba colocando o primeira elemento da lista como a raíz
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl insertTree Nil

-- Union Type

data Either a b = Left a | Rigth b

funcEither :: (a -> c) -> (b -> c) -> Either a b -> c
funcEither f g (Left x) = f x
funcEither f g (Rigth y) = g y

applyLeft :: (a -> c) -> Either a b -> c
applyLeft f (Left x)  = f x
applyLeft f (Rigth y) = error "applyLeft applied to Rigth"


data ListaPoli t = Null | Const t (ListaPoli t)

paraListaBib :: ListaPoli t -> [t]
paraListaBib (Null)      = []
paraListaBib (Const n l) = n : paraListaBib l

-- 3 formas de lidar com erros:

-- 1. Interromper a computação no instante do erro, essencialmente, usando error
-- É ruim porque eprde qualquer informação valiosa na computação já realizada

-- 2. Escolher valores "Dummy". Em alguns casos, não fará sentindo
-- um valor "Dummy", logo, você adiciona um parâmetro na função
-- que servirá como esse valor dummy. O problema dessa abordagem (2.)
-- É que não saberemos quando de fato ocorreu um erro. Exemplo:

hd :: a-> [a]-> a
hd y (x:_) = x
hd y [] = y

-- 3. Usar um type error para capturar e processar erros:

data MMaybe a = mJust a | mNothing
                        deriving (Show, Eq, Read, Ord)

errDiv :: Integer-> Integer-> MMaybe Integer
errDiv n m
 |(m/=0) = mJust (n‘div‘m)
 |otherwise = mNothing

novaDiv :: Integer -> Integer -> String
novaDiv x y = case errDiv x y of
                            mNothing -> "Divisão por Zero, Erro!!"
                            MJust n -> "Resultado da divisao: " ++ show n

-- o Maybe nos permite lançar um erro e com isso, podemos transmitir, com mapMaybe
-- ou Capturar esse erro, com maybe

mapMaybe :: (a-> b)-> Maybe a-> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

maybe :: b-> (a-> b)-> Maybe a-> b
maybe n f Nothing = n
maybe n f (Just x) = f x

maybe 56 (1+) (mapMaybe (*3) (errDiv 9 0))
=maybe 56 (1+) (mapMaybe(*3)Nothing)
=maybe 56 (1+) Nothing
=56

maybe 56 (1+) (mapMaybe (*3) (errDiv 9 1))
=maybe56(1+)(mapMaybe(*3)(Just9))
=maybe56(1+)(Just27)
=1+27
=28
