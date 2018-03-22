-- 01 Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.
chLado:: Double -> Double -> Double -> Bool
chLado a b c = abs(b - c) < a &&  b + c > a
ehTriangulo  :: Double -> Double -> Double -> Bool
ehTriangulo x y z = chLado x y z && chLado y x z && chLado z x y

-- 02 Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.
tipoTriangulo :: Double -> Double -> Double -> String

tipoTriangulo x y z 
 | not (ehTriangulo x y z) = "Nao Triangulo"
 | x == y && y == z  = "Triangulo Equilatero"
 | x == y || y == z || z == x  = "Triangulo Isosceles"
 | x /= y && y /= z && z /= x  = "Triangulo Escaleno"

-- 03 Implemente uma função que faz a multiplicação etíope entre dois números.
par :: Integer -> Bool
par n = n `mod` 2 == 0

etiope :: Integer -> Integer -> Integer
etiope m n
  | m == 1 = n
  | par m = etiope (m `div` 2) (n*2)
  | otherwise  = n + etiope (m `div` 2) (n*2)


-- 04 Faça uma função que determine se um número é primo.
primo :: Integer -> Integer -> Bool
primo n m
 | m==1 = True
 |(n `rem` m) == 0 = False 
 | otherwise = primo n (m-1)
 
ehPrimo :: Integer -> Bool
ehPrimo n = primo n (n-1)

-- 05 Faça uma função que calcule a soma dos dígitos de um número.
somaDigitos :: Integer -> Integer
somaDigitos 0 = 0
somaDigitos n =  (n `mod` 10) + somaDigitos(n `div` 10)

-- 06 Faça uma função que calcule a persistência aditiva de um número.
persAditiva :: Integer-> Integer
persAditiva n
  | n < 10 = 0
  | otherwise = 1 + persAditiva (somaDigitos n)

-- 07 Faça uma função que calcule o coeficiente binomial de (m,n).
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = fatorial' n 1
  where
    fatorial' 1 r = r
    fatorial' n r = fatorial' (n-1) (n*r)
    
cfBinomial :: Integer -> Integer -> Integer
cfBinomial n k
  | n < k = 0
  | otherwise = (fatorial n) `div` ((fatorial k) * fatorial (n-k))

-- 08 Faça uma função que calcule o elemento (i,j) do triângulo de pascal.
triPascal :: Integer -> Integer -> Integer
triPascal i j = cfBinomial i j

-- Validação Simples
main = do
  let ex1a = ehTriangulo 10.0 9.0 5.0
  let ex1b = ehTriangulo 8.0 4.0 3.0
  
  let ex2a = tipoTriangulo 10.0 9.0 5.0
  let ex2b = tipoTriangulo 8.0 4.0 3.0
  let ex2c = tipoTriangulo 8.0 8.0 8.0
  let ex2d = tipoTriangulo 5.0 5.0 3.0
  
  let ex3a = etiope 13 21
  let ex3b = etiope 17 14
  let ex3c = etiope 2  5
  
  let ex4a = ehPrimo 19
  let ex4b = ehPrimo 20
  let ex4c = ehPrimo 15
  
  let ex5a = somaDigitos 19
  let ex5b = somaDigitos 564894
  let ex5c = somaDigitos 390
  
  let ex6a = persAditiva 8987989
  let ex6b = persAditiva 3
  let ex6c = persAditiva 9876
  
  let ex7a = cfBinomial 50 3 
  let ex7b = cfBinomial 11  3
  let ex7c = cfBinomial 1 2
  
  --Linha 5
  let ex8a = triPascal 5 0
  let ex8b = triPascal 5 1 
  let ex8c = triPascal 5 2
  let ex8d = triPascal 5 3
  let ex8e = triPascal 5 4
  let ex8f = triPascal 5 5
  
  print (ex8a,ex8b,ex8c,ex8d,ex8e,ex8f)

