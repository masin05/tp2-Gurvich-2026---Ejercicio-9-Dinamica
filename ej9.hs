import Data.Array

-- Función principal que calcula la longitud del subpalíndromo máximo
longitudSubpalindromoMaximo :: String -> Int
longitudSubpalindromoMaximo "" = 0
longitudSubpalindromoMaximo s = maxLen
  where
    n = length s
    -- Convertimos la cadena en un arreglo indexado de 0 a n-1
    arr = listArray (0, n - 1) s

    -- Definimos la tabla booleana P como un arreglo de 2 dimensiones: Array (Int, Int) Bool
    -- Gracias a la pereza de Haskell, podemos definirlo de forma recursiva basada en sus dependencias.
    p :: Array (Int, Int) Bool
    p = listArray ((0, 0), (n - 1, n - 1)) [esPalindromo i j | i <- [0..n-1], j <- [0..n-1]]

    esPalindromo i j
      | i == j          = True                      -- Caso base: longitud 1
      | i + 1 == j      = arr ! i == arr ! j        -- Caso base: longitud 2
      | i > j           = True                      -- Fuera de rango (no usado)
      | otherwise       = (arr ! i == arr ! j) && (p ! (i + 1, j - 1)) -- Recurrencia

    -- Buscamos la longitud máxima entre todas las subcadenas que resultaron ser palíndromos
    maxLen = maximum [ j - i + 1 | i <- [0..n-1], j <- [i..n-1], p ! (i, j) ]

-- Ejemplo de uso:
-- *Main> longitudSubpalindromoMaximo "accaba"
-- 4  (corresponde a "acca")
