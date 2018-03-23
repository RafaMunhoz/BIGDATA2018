showMat :: [[Int]] -> String
showMat = unlines . map (unwords . map show)

-- 01 Faça uma função que gere uma matriz identidade de tamanho n.
matId n = [ [fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

-- 02 Faça uma função que calcule a soma da diagonal principal de uma matriz.

-- 03 Faça uma função que calcule a soma da diagonal secundária de uma matriz.
secondary i j mat sum
  | j==0 = sum
  | otherwise = (mat!(i,j)) + secondary (i+1) (j-1) mat sum
  
sumsecondary mat = secondary 0 6 mat 0

-- Validação Simples
main = do
  putStr $ sumsecondary $ matId 6
