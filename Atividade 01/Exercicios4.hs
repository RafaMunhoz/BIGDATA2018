showMat :: [[Int]] -> String
showMat = unlines . map (unwords . map show)

-- 01 Faça uma função que gere uma matriz identidade de tamanho n.
matId n = [ [fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

-- 02 Faça uma função que calcule a soma da diagonal principal de uma matriz.
somaDiag1:: [[Int]]->Int
somaDiag1  mat
    | nrows  == ncols  =  sum  [ mat!!i!!i  | i <- [0..nrows-1]  ]
    | otherwise =  error "matriz nao e quadrada"
    where
        nrows = length mat
        ncols = length (mat!!0)        

-- 03 Faça uma função que calcule a soma da diagonal secundária de uma matriz.
somaDiag2::[[Int]]->Int
somaDiag2 mat
  | nrows  == ncols  =  sum [ mat!!i!!(ncols-1-i)  | i <- [0..nrows-1]  ]
  | otherwise =  error "matriz nao e quadrada"
  where
    nrows = length mat
    ncols = length (mat!!0)       

-- Validação Simples
main=do
print (somaDiag2 [[3,7,2,12], [4,5,4,6], [7,11,3,6], [14,1,2,2]])
