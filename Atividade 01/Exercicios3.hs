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

-- 02: Crie uma função projectEuler5 que retorna o primeiro número natural que retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.
-- Minha versao
encontra :: Integer -> Integer
encontra count
  |divisivel20 count = count
  |otherwise = encontra (count+20)  
projectEuler5 = encontra 20

--Versão melhorada (encontrada no site https://wiki.haskell.org/Euler_problems/1_to_10)
projectEuler5 = foldr1 lcm [1..20]
--A foldr1 ela aplica a partir do fim da lista uma operação entre os itens, nesse caso lcm, o menor multiplo comum, 
--dessa maneira encontrando o menor multiplo entre 20 e 19 que é 380, entre 380 e 18, 3420, entre 3420 e 17, 58140, 
--entre 58140 e 16, 232560, até encontrar entre 21162960 e 11 que é a resposta 232792560. (ele continua até 232792560 e 1)
  
-- 03: Crie a lista de números de Fibonacci utilizando uma função geradora.
fibs = 0 : 1 : prox fibs
  where
    prox (x:t@(y:resto)) = (x+y) : prox t

-- 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares dos valores que não excedem 4.000.000. (Project Euler 2)
projectEuler2 = sum [ x | x <- takeWhile (<= 4000000) fibs, even x]
    
-- 05: Faça uma função para calcular o produto escalar entre dois vetores.
escalarProd :: Num a => [a] -> [a] -> a 
escalarProd a b 
  | length a == length b = sum (zipWith (*) a b)
  | otherwise = error "Os tamanhos dos vetores devem ser iguais"

-- 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.
collatz :: Integer -> Integer
collatz x
     | mod x 2 == 0 = div x 2
     | otherwise =  3 * x + 1

-- 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
collatzLoop :: Integer -> Integer -> Integer
collatzLoop cont x 
  | x == 1 = cont
  | otherwise = collatzLoop (cont+1) $ collatz x
  
collatzLen :: Integer -> Integer
collatzLen x = collatzLoop 1 x

-- 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz. (Project Euler 14)
colmax x n = x `max` (collatzLen n, n) 
search nums = foldl colmax (1,1) nums
projectEuler14 = search [1..1000000]
-- Retorna o tamanho da maior lista e o numero que a gerou (525,837799), demorou muito 
--Versão encontrada no site https://wiki.haskell.org/Euler_problems/11_to_20
main = print soln
    where
        s1 = search [2..500000]
        s2 = search [500001..1000000]
        soln = s2 `par` (s1 `pseq` max s1 s2)
        
--Versão proposta pelo professor
import Data.List
import Data.Ord
import qualified Data.Array as Array

nextCollatz :: Integer -> Integer
nextCollatz x
  | even x    = x `div` 2
  | otherwise = 3*x + 1
     
memoLenCollatz = lengths
  where
    lengths = Array.listArray (1, 1000000) [memoLen x | x <- [1..1000000]]
    memoLen 1 = 1 
    memoLen 2 = 2
    memoLen 3 = 2
    memoLen n 
      | next <= 1000000 = 1 + lengths Array.! next
      | otherwise       = 1 + memoLen next
      where next = nextCollatz n
    
numeros = [1..1000000]    

lengths = take 1000000
        $ Array.assocs memoLenCollatz   -- retorna lista de tuplas (indice, valor)

maior = fst $ maximumBy (comparing snd) lengths  -- retorna o maior elemento da lista comparado pelo segundo elemento da array

main = do 
  print maior

-- Validação Simples
main = do
  let test = encontra 1

  print (test)
