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
etiope :: Integer -> Integer -> Integer
etiope m n =

while( m!=0 ){   // m vai ser dividido sucessivamente por 2, até chegar em 0
      if( m%2!=0 ){  // se m é par vou somar o valor de n correspondente
        mult += n;
      }
      m /= 2;         // próxima linha da tabela, m/2 e n*2
      n *= 2;
    }


-- 04 Faça uma função que determine se um número é primo.
primo :: Int -> Int -> Bool
primo n 1 = True
primo n m = if (n `rem` m) == 0 then False else primo n (m-1)

-- 05 Faça uma função que calcule a soma dos dígitos de um número.

-- 06 Faça uma função que calcule a persistência aditiva de um número.

-- 07 Faça uma função que calcule o coeficiente binomial de (m,n).

-- 08 Faça uma função que calcule o elemento (i,j) do triângulo de pascal.

-- Validação Simples
main = do
  let ex1a = ehTriangulo 10.0 9.0 5.0
  let ex1b = ehTriangulo 8.0 4.0 3.0
  
  let ex2a = tipoTriangulo 10.0 9.0 5.0
  let ex2b = tipoTriangulo 8.0 4.0 3.0
  let ex2c = tipoTriangulo 8.0 8.0 8.0
  let ex2d = tipoTriangulo 5.0 5.0 3.0
  
  print (ex2a)
  print (ex2b)
  print (ex2c)
  print (ex2d)
