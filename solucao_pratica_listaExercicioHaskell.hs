-- Questao 2

sublistas :: [a] -> [[a]]
sublistas []      =  [[]]
sublistas (x:xs)  =  [ x:ys | ys <- sublistas xs ] ++ sublistas xs

-- Questao 3


poli :: Integer -> Integer -> Integer -> Integer -> Integer
poli m n p = (\x ->  (m * x * x) + (n * x) + p)


listaPoli :: [(Integer,Integer,Integer)] -> [Integer->Integer]
listaPoli l = [poli x y z | (x,y,z) <- l ]

appListaPoli :: [Integer->Integer] -> [Integer] -> [Integer]
appListaPoli [] [] = []
appListaPoli (x:xs) (y:ys) = x y: appListaPoli xs ys

appListaPoli lPoli lInt   = [f x | (f,x) <- zip lPoli lInt]

-- a

matriz = [[1,2,2,3],[6,7,8,9]]
mQuadrada = [[1,2,2,3],[6,7,8,9],[7,6,5,9],[5,4,8,2]]

validaMatriz :: [[Integer]] -> Bool
validaMatriz m = listaValoresIguais(map length m)

listaValoresIguais :: [Int] -> Bool
listaValoresIguais (x:[]) = True
listaValoresIguais (x:y:[]) = x == y
listaValoresIguais (x:y:ys) = x == y && listaValoresIguais (ys)

-- b

diagPrincipal :: [[Integer]] -> [Integer]
diagPrincipal l = reverse (extraiPrincipal l (length(l)-1))

extraiPrincipal :: [[Integer]] -> Int -> [Integer]
extraiPrincipal l 0 = [((l!!0)!!0)]
extraiPrincipal l i = (l!!i)!!i : extraiPrincipal l (i-1)

-- c

permutaValores :: Int -> Int -> [t] -> [t]
permutaValores x y l =
 init (take x l) ++ 
  [(take y l)!!(y-1)] ++ 
  (take (y-x-1) (drop x l)) ++
  [(take x l)!!(x-1)] ++ 
  (drop y l)

  
-- Questao 4

type Codigo = Int
data Voto = Presidente Codigo | 
            Senador Codigo | 
                Deputado Codigo
                 | Branco  deriving (Show)
type Urna = [Voto]
type Apuracao = [(Voto, Integer)]


instance Eq Voto where
   (Presidente p1) == (Presidente p2) = p1 == p2
   (Senador s1)   == (Senador s2)   = s1 == s2  
   (Deputado d1) == (Deputado d2)  = d1 == d2
   Branco == Branco = True
   _ == _ = False
   
umaUrna= [(Presidente 1), (Presidente 1) , 
 (Senador 1),(Senador 1),(Senador 1),
 (Presidente 2), (Presidente 3),
 (Deputado 1), (Deputado 1),(Deputado 1),
 (Deputado 1), (Deputado 1),(Deputado 1), 
 (Deputado 1), (Deputado 1),(Deputado 1),
 Branco, Branco, (Presidente 1)
 ]

umaApuracao = [((Presidente 1), 1), ((Senador 1),3)] 
 
totalVotos :: Urna -> Voto  -> Int
totalVotos u v = length([ x | x <- u , x == v ]) 


apurar :: Urna -> Apuracao -> Apuracao
apurar (v:[]) ap = atualizarApuracaoVoto v ap
apurar (v:vs) ap = apurar vs (atualizarApuracaoVoto v ap)     

apurar2 [] = []
apurar2 (x:xs) = (x, 1 + totalVotos xs x): apurar2 (filter (/=x) xs) 

atualizarApuracaoVoto voto ap
 | votoRegistrado ap voto = atualizarApuracao ap voto
 | otherwise = ap ++ [(voto,1)]


atualizarApuracao :: [(Voto, Integer)] -> Voto -> [(Voto, Integer)]
atualizarApuracao ([]) voto = []
atualizarApuracao (x:xs) voto
  | fst(x) == voto = ((voto, (snd x) + 1)):xs
  | otherwise = x: atualizarApuracao xs voto

  
votoRegistrado :: [(Voto, Integer)] -> Voto -> Bool
votoRegistrado [] voto = False
votoRegistrado (x:xs) voto = fst(x) == voto || votoRegistrado xs voto
  


-- Questao 5
{--

data Pilha t = Pilha  [t] | PilhaVazia deriving Show
 
 push :: t -> Pilha t -> Pilha t
 push t PilhaVazia = Pilha [t] 
 push t (Pilha x) =(Pilha (t:x))
 
 pop :: Pilha t -> Pilha t
 pop PilhaVazia = error "Pilha Vazia" 
 pop (Pilha (x:xs)) = (Pilha xs)
 
 top :: Pilha t -> t
 top PilhaVazia = error "Pilha Vazia" 
 top (Pilha (x:xs)) = x
 --}


