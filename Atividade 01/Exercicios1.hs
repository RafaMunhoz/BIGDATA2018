-- 01 Execute as seguintes operações utilizando o menor número de parênteses:

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
mult35 :: Integer -> Bool
mult35 x = mult3 x && mult5 x

-- 06 Faça uma função que recebe um tipo Integer e retorna ele dividido por 2:
divd2 :: Integer -> Double
divd2 x = x/2


-- 07 Faça uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo utilizando a identidade:


-- 08 Crie uma lista de anos bissextos desde o ano 1 até o atual.

-- 09 Encontre os 10 primeiros anos bissextos.

-- 09 Encontre os 10 últimos anos bissextos (dica: use a função length para determinar o tamanho da lista).

-- 10 Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.

-- 11 Crie um concatenador de strings que concatena duas strings separadas por espaço.

-- 12 Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.

-- Validação Simples
main = do
  let ex1a = mult35 12
  let ex1b = mult35 15
  
  print(ex1a,ex1b)


