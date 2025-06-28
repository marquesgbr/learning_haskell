{-
1. (0,2 points) Usando recursão, implemente a função sumTo, de modo que sumTo n cal-
cula o valor de 1 + 2 + ... + n
-}
sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

{-
2. (0,2 points) Defina a função potencia, de modo que potencia n k calcula n elevado a k.
Use recursão.
-}
potencia :: Integer -> Integer -> Integer
potencia 0 0 = error "0 elevado a 0 não é definido"
potencia _ 0 = 1
potencia n 1 = n
potencia n k
    | k < 0 = error "Essa função não aceita expoentes negativos; calcule 'potencia n k' e divida 1 pelo resultado"
    | otherwise = n * potencia n (k - 1)

{-
3. (0,2 points) Usando recursão, compute os coeficientes binomiais dados pelas seguintes
equações
B(n, k) = B(n − 1, k) + B(n − 1, k − 1)
B(n, 0) = 1
B(0, k) = 0, quando k > 0
Dica: usar casamento de padrão pode ser de grande ajuda.
-}
binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k
    | k < 0 = error "k pode ser não-negativo"
    | n < 0 = error "n deve ser não-negativo"
    | k > n = 0
    | otherwise = binomial (n - 1) k + binomial (n - 1) (k - 1)

{-
4. (0,2 points) Os números de Tribonacci são dados pelas seguintes equações
T(1) = 1
T(2) = 1
T(3) = 2
T(n + 1) = T(n) + T(n − 1) + T(n − 2)
Implemente uma função recursiva eficiente que calcula T n. Considere o uso de uma
função auxiliar que leva em consideração o conceito de recursão de cauda.
-}
tribonacci :: Integer -> Integer
tribonacci n
    | n <= 0 = error "n deve ser maior que 0"
    | otherwise = tribonacciAux n 1 1 2
    where
        tribonacciAux 1 a _ _ = a 
        tribonacciAux 2 _ b _ = b 
        tribonacciAux 3 _ _ c = c
        tribonacciAux m a b c = tribonacciAux (m - 1) b c (a+b+c) 


{-
5. (0,2 points) Defina a função
squares :: Int −> [Integer]
que encontra os n primeiros quadrados que começam e terminam com o mesmo dígito.
Por exemplo,
squares 9 ⇝ [1,4,9,121,484,676,1521,1681,4624]
-}
squares :: Int -> [Integer]
squares n = take n [n^2 | n <- [1..], sameFirstLast (n^2)]
    where
        sameFirstLast num = let numStr = show num 
                            in head numStr == last numStr
