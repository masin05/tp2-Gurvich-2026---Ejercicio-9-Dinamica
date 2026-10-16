import Data.Array

-- Función principal que calcula la longitud del subpalíndromo máximo
longitudSubpalindromoMaximo :: String -> Int
longitudSubpalindromoMaximo "" = 0
longitudSubpalindromoMaximo xs = maxLen
  where
    n = length xs
    -- Convertimos la cadena en un arreglo indexado de 0 a n-1
    arr = listArray (0, n - 1) xs

    -- Definimos la tabla booleana matriz como un arreglo de 2 dimensiones: Array (Int, Int) Bool
    matriz:: Array (Int, Int) Bool
    matriz = listArray ((0, 0), (n - 1, n - 1)) [esPalindromo inicio fin | inicio <- [0..n-1], fin <- [0..n-1]]

    esPalindromo inicio fin
      | inicio == fin          = True                                                -- Caso base: longitud 1
      | inicio + 1 == fin      = arr ! inicio == arr ! fin                           -- Caso base: longitud 2
      | inicio > fin           = True                                                -- Fuera de rango (no usado)
      | otherwise       = (arr ! inicio == arr ! fin) && (matriz ! (inicio + 1, fin - 1)) -- Recurrencia

    -- Buscamos la longitud máxima entre todas las subcadenas que resultaron ser palíndromos
    maxLen = maximum [ fin - inicio + 1 | inicio <- [0..n-1], fin <- [i..n-1], matriz ! (inicio, fin) ]

-- Ejemplo de uso:
-- *Main> longitudSubpalindromoMaximo "accaba"
-- 4  (corresponde a "acca")
