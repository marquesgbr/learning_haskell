{- Where
As definições locais com where são declaradas após a linha da função principal à 
qual elas pertencem. Elas são visíveis apenas dentro da função onde foram definidas. 
-}


sumDoubleSquare :: Int -> Int -> Int 
sumDoubleSquare x y = dSqX + dSqY 
  where dSqX = 2 * (x * x) 
        dSqY = 2 * (z * z)

-- Definicoes com where tambem podem ter seus proprios parametros
sumDoubleSquare2 :: Int -> Int -> Int 
sumDoubleSquare2 x y = dSq x + dSq y 
  where dSq z = 2 * (z * z) -- z -> parametro de dSq

{- 
Resumindo: <Definicoes> where <Definicoes do where>; que nem na matematica
onde temos no final da definicao de um conjunto/funcao a explicacao dos parametros
-}


{- Let
 Expressões let permitem definir valores locais dentro de uma expressão. 
 Isso significa que a definição local é visível apenas na expressão 
 que a segue, depois da palavra-chave in. Separamos os Let com ";"
-}

let x = 3 + 2 in x^2 + 2*x - 4

let x = 3 + 2; y = 6 - 2 in x^2 + 2*x - y


sumSquares :: Int -> Int -> Int 
sumSquares x y 
  = let sqX = x * x 
        sqY = y * y 
    in sqX + sqY
-- sqX e sqY definidos localmente dentro do corpo da funcao e usados na expressao final

{- Pattern Matching
Regras:
    1. As entradas devem ser tipo-compativeis
    2. Nao pode haver ambiguidade, pois a ordem é usada para resolver conflitos 
    3. Os casos devem ser exaustivos -> evitar funcoes parciais (opcional)
-}


ehImpar :: Int -> Bool
| n<=0 = False
| otherwise = ehPar (n-1)

ehPar :: Int -> Bool
| n<0 = False
| n == 0 = True
| otherwise = ehImpar (n-1)


totalVendas :: Int -> Int
totalVendas n
| n == 0 = vendas 0
| otherwise = totalVendas(n-1) + vendas n

maxi :: Int -> Int -> Int
maxi m n
| m >= n = m
| otherwise = n

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = maxi (maxVendas(n-1)) (vendas n)

{- Tail Recursion
Nesse tipo de recursão, o resultado final da chamada é o resultado final do cálculo
da função. É mais otimizado pelo fato de não precisar empilhar, pois os 
resultados parciais já são acumulados. O GHC já tenta fazer essa otimização 
quando possível
-}

tailFat :: Int -> Int -> Int
tailFat 0 x = x
tailFat n x = tailFat (n-1) (n*x)

fat n = tailFat n 1

{-
Nesse exemplo com tailFat, x é o resultado acumulado até então (começa em 1), enquanto
n é o número de "iterações" (chamadas) restantes. Ele para de fazer chamadas quando n 
chega a zero. Coincidentemente, o número de chamadas restantes (n) 
é igual ao número que será acumulado em x a cada chamada 
-}
