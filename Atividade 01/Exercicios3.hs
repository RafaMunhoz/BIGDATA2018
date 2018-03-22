-- 01 Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo.
chLado:: Double -> Double -> Double -> Bool
chLado a b c = abs(b - c) < a &&  b + c > a
ehTriangulo  :: Double -> Double -> Double -> Bool
ehTriangulo x y z = chLado x y z && chLado y x z && chLado z x y

-- 02 Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.
tipoTriangulo :: Double -> Double -> Double -> String

tipoTriangulo x y z 
 | ehTriangulo x y z = "Nao Triangulo"
 | x == y && y == z  = "Triangulo Equilatero"
 | x == y || y == z || z == x  = "Triangulo Isosceles"
 | x /= y && y /= z && z /= x  = "Triangulo Escaleno"

-- 03 Implemente uma função que faz a multiplicação etíope entre dois números.


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

