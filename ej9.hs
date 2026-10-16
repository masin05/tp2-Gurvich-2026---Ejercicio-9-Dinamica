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

-- Caso 1: Cadena vacía (Caso límite básico)
ejemplo1 :: Int
ejemplo1 = longitudSubpalindromoMaximo "" 
-- Resultado esperado: 0

-- Caso 2: Cadena corta con subpalíndromo par ("acca") y longitud impar general ("accaba")
ejemplo2 :: Int
ejemplo2 = longitudSubpalindromoMaximo "accaba" 
-- Resultado esperado: 4 
-- (Explicación para el profe: El subpalíndromo máximo es "acca" de longitud 4).

-- Caso 3: Palíndromo completo impar en el centro
ejemplo3 :: Int
ejemplo3 = longitudSubpalindromoMaximo "abacccabaada" 
-- Resultado esperado: 9 
-- (Explicación para el profe: El subpalíndromo máximo es "abacccaba" de longitud 9).

-- Caso 4: Letras todas distintas (Ningún palíndromo mayor a 1)
ejemplo4 :: Int
ejemplo4 = longitudSubpalindromoMaximo "abcdef" 
-- Resultado esperado: 1 
-- (Explicación para el profe: Cualquier caracter individual es un palíndromo de longitud 1).

-- Caso 5: Toda la cadena es un palíndromo simétrico ("reconocer" o "anitalavalatina")
ejemplo5 :: Int
ejemplo5 = longitudSubpalindromoMaximo "reconocer" 
-- Resultado esperado: 9 
-- (Explicación para el profe: Toda la palabra completa es un palíndromo).
