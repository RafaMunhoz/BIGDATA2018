-- 01: Crie uma função divisivel20 x que retorna verdadeiro se x for divisível por todos os números de 1 a 20.
divisivelN :: Integer -> Integer -> Bool
divisivelN x n = x `mod` n == 0

divisivelRange :: Integer -> Integer -> Integer -> Bool
divisivelRange x n count
  | not(divisivelN x count) = False
  | count == n = True
  | otherwise = divisivelRange x n (count+1) 

divisivel20 :: Integer -> Bool
divisivel20 n = divisivelRange n 20 1

encontra count
  |divisivel20 count = count
  |otherwise = encontra (count+1)

-- 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.

-- 03: Crie a lista de números de Fibonacci utilizando uma função geradora.

-- 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)

-- 05: Faça uma função para calcular o produto escalar entre dois vetores.

-- 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.

-- 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.

-- 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)



-- Validação Simples
main = do
  let ex1a = encontra 1

  print (ex1a)
