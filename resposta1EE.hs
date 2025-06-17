
type Equipamento = String
type Uso = (Equipamento, Int)
type ListaUso = [Uso]

{-
let e1 = ("maquina de lavar", 2)
let e2 = ("cafeteira", 1)
let e3 = ("lava loucas", 2)
let e5 = ("asp", 0 )
let e6 = ("bat", 25)
let l = [e1, e2, e3, e1, e2]
let l1 = [e5, e6]
let tr1 = ("maquina de lavar", 20)
let tr2 = ("cafeteira", 3)
let tr3 = ("lava loucas", 15)
let trf = [tr1, tr2, tr3]
inv l
inv l1
duracaoDe "cafeteira" l
bemFormada l
bemFormada l1
removerEqp "cafeteira" l
removerEqp "cafeteira" l1
definidoEm trf l
precoDe l trf
precoDe l [("mixer", 4)] 

-}
{-
1(a)> inv l

1(b)> duracaoDe "cafeteira" l

1(c)> bemFormada l

1(d)> removerEqp "cafeteira" l
[("maquina de lavar",2),("lava loucas",2),("maquina de lavar",2)]


1(e)> definidoEm trf l
True

1(f)> precoDe l trf
116
1(f)> precoDe l [("mixer", 4)]
*** Exception : Tarifa nao definida



main = do inv l
          duracaoDe "cafeteira" l
          bemFormada l
          bemFormada l1
          removerEqp "cafeteira" l
          removerEqp "cafeteira" l1
          definidoEm trf l
          precoDe l trf
          precoDe l [("mixer", 4)] 




-}

inv :: ListaUso -> Bool
inv []      = True
inv (x:xs)  = snd x > 0 && inv xs 

duracaoDe :: Equipamento -> ListaUso -> Int
duracaoDe eqp []   = 0
duracaoDe eqp (x:xs)
  | fst x == eqp  = snd x + duracaoDe eqp xs
  | otherwise     = duracaoDe eqp xs

bemFormada :: ListaUso -> Bool
bemFormada lUso = inv lUso && ( usoAbaixo24Horas . duracaoEqps ) lUso

usoAbaixo24Horas :: ListaUso -> Bool
usoAbaixo24Horas  [] = True
usoAbaixo24Horas  l  = (filter (\u -> snd u > 24) l )  == []

duracaoEqps :: ListaUso -> ListaUso
duracaoEqps []  = []
duracaoEqps (x:xs) = 
                (fst x, snd x + totalEquip (fst x) xs) : 
                   duracaoEqps [u | u <- xs, fst u /= fst x]
-- [("cafeteira, 1"), ("ml", 2) , ("cft", 1), ("ll", 3), ("cft",1)]
-- [("cafeteira", 3), ("ml", 2) , ("ll", 3) ]
--

totalEquip :: Equipamento -> ListaUso -> Int
totalEquip str [] = 0
totalEquip str (y:ys) 
  | fst y == str   = snd y + totalEquip str ys
  | otherwise      = totalEquip str ys

removerEqp :: Equipamento -> ListaUso -> ListaUso
removerEqp eqp l = [u | u <- l, fst u /= eqp]

type Preco   = Int
type Tarifa  = (Equipamento, Preco)
type Tarifas = [Tarifa]

definidoEm :: ListaUso -> Tarifas -> Bool
definidoEm lu trfs = 
    let l1 = listaEqpSemRepeticao lu
        l2 = listaEqpSemRepeticao trfs 
    in
            subtracao l2 l1 == []

subtracao :: Eq a => [a] -> [a] -> [a]
subtracao [] l = l
subtracao l [] = []
subtracao (x:xs) l  = subtracao xs (filter (/= x) l)

{-
subtracao [1,2,3] [1,2,3]
= subtracao [2,3] [2,3]
= subtracao [3] [3]
= subtracao [] []
= []

substracao [1,2,3,4] [1,2]
= subtracao [2,3,4] [2]
= subtracao [3,4] []
= []

subtracao [1,2] [1,2,3,4]
= subtracao [2] [2,3,4]
= subtracao [] [3,4]
= [3,4]
 
-}

listaEqpSemRepeticao :: ListaUso -> [Equipamento]
listaEqpSemRepeticao [] = []
listaEqpSemRepeticao (x:xs) = fst x : listaEqpSemRepeticao ([u | u <- xs, fst u /= fst x]) 


precoDe :: ListaUso -> Tarifas -> Preco
precoDe [] lTrfs    =  0 
precoDe (x:xs) lTrfs = 
    ((tarifaEqp x lTrfs) * snd x) + precoDe xs lTrfs
  
tarifaEqp :: Uso -> Tarifas -> Int
tarifaEqp (eq,h) [] = error "Tarifa nao definida"
tarifaEqp (eq, h) (t:ts) 
 | eq == fst t   = snd t
 | otherwise     = tarifaEqp (eq,h) ts



--

vendas 0 = 1
vendas 1 = 5
vendas 2 = 0
vendas 3 = 4
vendas 4 = 0


zeroVendas1 n = length [() | i <- [0..n], vendas i == 0]
zeroVendas2 n = (length . filter (==0) . map vendas) [0..n]