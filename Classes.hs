class Eq t where
    (==) :: t -> t -> Bool

instance Eq Bool where
    (==) True True = True
    (==) False False = True
    (==) _ _         = False


data Shape = Circle Float | Rectangle Float Float

class Info a where
    examples :: [a]
    size :: a -> Int

instance Info Bool where
    examples = [True, False]
    size True = 4
    size False = 5

instance Info Int where
    examples = [-100 .. 100]
    size _ = 1

instance Info Char where
    examples = ['A' .. 'Z']
    size _  = 1 

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle l h) = l*h

instance Info Shape where
    examples = [Circle 5.4, Recntagle 4.6 11]
    size = round . area 

-- Não entendi essa parte seguinte:

instance Info a => Info [a] where
 examples = [ [] ] ++
            [[x]|x<-examples]++
                [[x,y]|x<-examples,y<-examples]
 size = foldr (+) 1 . map size

instance (Eq a,Eq b) => Eq (a,b) where
(x,y) == (z,w) = x==z && y==w

-- Definições Default:

class Eq a where
 (==), (/=) :: a-> a-> Bool
 x/=y =not(x==y)
 x==y =not(x/=y)

 -- Por exmeplo, poderia ter feito

class Info a where
 examples :: [a]
 size :: a-> Int
 size _ = 1

-- Assim, poderia reescrever size somente do tipo Bool, pro exmeplo:

instance Info Int where
    examples = [-100..100]
instance Info Char where
    examples = [’a’,’A’,’z’,’Z’,’0’,’9’]
instance Info Bool where
    examples = [True,False]
    size True = 4
    size False = 5

-- Eu poderia transformar a definicao default de /=, por exemplo, 
-- Em uma definição top-level, ou seja, uma função "normal" fora da classe
-- Dessa forma, eu tiro a possibilidade de ficar reescrevendo ela
-- Dentro das instâncias da classe

-- Classes Derivadas: Ord está HERDANDO os operadores de Eq

data Ordering = LT | EQ | GT

class Eq a => Ord a where
 (<), (<=), (>), (>=) :: a-> a-> Bool
 max, min :: a-> a-> a
 compare :: a-> a-> Ordering

  (<)  a1 a2 =  a2 > a1
  
  (>)  a1 a2 =  a2 < a1

  (<=) a1 a2 =  a1 < a2  ||  a1 == a2
  
  (>=) a1 a2 =  a1 > a2  ||  a1 == a2
  
  max a1 a2
   
   |  a1 /= a2 && a1 > a2  = a1
   |  otherwise            = a2

  min a1 a2
  
   |  a1 /= a2 && a1 < a2  = a1
   |  otherwise            = a2
  
  compare a1 a2
   
   |  a1 < a2    = LT
   |  a1 > a2    = GT
   |  otherwise  = EQ

-- Outra forma seria definir:

{-
 x<=y =comparexy/=GT
 x< y =comparexy==LT
 x>=y =comparexy/=LT
 x> y =comparexy==GT
-}