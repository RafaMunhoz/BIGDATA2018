-- 01 Execute as seguintes operações utilizando o menor número de parênteses:2*3+5 2+2*3+1 3^4+5 * 2^5 +1

-- 02 Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 e False caso contrário.
mult3 :: Integer -> Bool
mult3 x = (x `mod` 3) == 0

-- 03 Faça uma função mult5 x que retorne True caso a entrada seja múltiplo de 5 e False caso contrário.
mult5 :: Integer -> Bool
mult5 x = (x `mod` 5) == 0

-- 04 Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.
mult35 :: Integer -> Bool
mult35 x = mult3 x && mult5 x

-- 05 Faça um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E múltiplo de 2), e False caso contrário.
testNum :: Integer -> Bool
testNum x 
  | x < -1 = True
  | otherwise = False

-- 06 Faça uma função que recebe um tipo Integer e retorna ele dividido por 2:
div2d :: Integer -> Double
div2d x = fromIntegral(x) / 2

-- 07 Faça uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo utilizando a identidade.
senTeta2 :: Double -> (Double,Double)
senTeta2 x = (sqrt ((1 - cos x) / 2),-sqrt ((1 - cos x) / 2))


-- 08 Crie uma lista de anos bissextos desde o ano 1 até o atual.
ehBissexto :: Integer -> Bool
ehBissexto x = False

listBissextos :: Integer -> Integer -> [Integer]
listBissextos x y = [1..10]

-- 09 Encontre os 10 primeiros anos bissextos.

-- 09 Encontre os 10 últimos anos bissextos (dica: use a função length para determinar o tamanho da lista).

-- 10 Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.

-- 11 Crie um concatenador de strings que concatena duas strings separadas por espaço.

-- 12 Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.

-- Validação Simples
main = do
  let ex1a = listBissextos 90 2
  let ex1b = listBissextos 60 2
  
  print(ex1a,ex1b)


