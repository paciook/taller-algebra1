{-  Dados dos números naturales n y m devuelve True si el numero n satisface la
 -  Conjetura de Collatz en menos de m pasos o iteraciones, y False en caso
 -  contrario. -}
satisfaceCollatz :: Integer -> Integer -> Bool
satisfaceCollatz 1 _ = True
satisfaceCollatz _ 1 = False {- Cuando m == 1 devuelvo False ya que el problema
                                indica que tienen que ser MENOS de m pasos -}
satisfaceCollatz n m | mod n 2 == 0 = satisfaceCollatz (div n 2) (m-1)
                     | otherwise = satisfaceCollatz (3*n + 1) (m-1)


{-  Dados dos números naturales n y m devuelve True si todos los números
 -  naturales desde 1 hasta n satisfacen la conjetura de Collatz en menos de
 -  m pasos o iteraciones y False en caso contrario. -}
satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta 1 _ = True 
satisfaceCollatzHasta n m = (satisfaceCollatz n m) &&
                            (satisfaceCollatzHasta (n-1) m)

{-  Dado un natural n que satisface la conjectura de Collatz devuelve la
 -  cantidad de términos pares que tiene la secuencia de Collatz desde a1 = n
 -  hasta llegar a 1. -}
cantidadTerminosPares :: Integer -> Integer
cantidadTerminosPares 1 = 0
cantidadTerminosPares n | mod n 2 == 0 = 1 + cantidadTerminosPares (div n 2)
                        | otherwise = cantidadTerminosPares (3*n + 1)

{-  Dado un natural n que satisface la conjectura de Collatz devuelve la
 -  cantidad de pasos desde a1 = n hasta llegar a 1. -}
largoSecuencia :: Integer -> Integer
largoSecuencia 1 = 0
largoSecuencia n | mod n 2 == 0 = 1 + largoSecuencia (div n 2)
                 | otherwise = 1 + largoSecuencia (3*n + 1)

{-  Dado un natural n que verifica que para todo 1 ≤ k ≤ n el número k
 -  satisface la conjetura de Collatz, devuelve el mínimo valor m, 1 ≤ m ≤ n,
 -  que genera la secuencia más larga de pasos desde a1 = m hasta llegar a 1.-}
secuenciaMasLargaHasta :: Integer -> Integer
secuenciaMasLargaHasta 1 = 1
secuenciaMasLargaHasta n | (largoSecuencia n > largoSecuencia anterior) = n
                         | otherwise = anterior
                         where anterior = secuenciaMasLargaHasta (n-1)
