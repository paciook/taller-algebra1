type Set a = [a]

insertarOrdenado :: Int -> Set Int -> Set Int
insertarOrdenado n [] = [n]
insertarOrdenado n (x:xs) | n <= x = n : x : xs
                          | otherwise = x : insertarOrdenado n xs

conjuntoEjemplo :: Set Int
conjuntoEjemplo = [1,3,4,7]

conjuntoEjemplo2 :: Set Int
conjuntoEjemplo2 = [1,3,4,7,9,15,20]

conjuntoVacio :: Set Int
conjuntoVacio = []

agregar :: (Ord a) => a -> Set a -> Set a
agregar n l | elem n l = l
            | otherwise = insertarOrdenado n l

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido (x:xs) l = elem x l && incluido xs l

iguales :: Set Int -> Set Int -> Bool
iguales l1 l2 = incluido l1 l2 && incluido l2 l1

union :: Set Int -> Set Int -> Set Int
union [] l = l
union (x:xs) l = agregar x (union xs l)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion (x:xs) l | elem x l = agregar x (interseccion xs l)
                      | otherwise = interseccion xs l

unionS :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionS s [] = s
unionS l (x:xs) = x : unionS l xs

mezcla :: Int -> Set (Set Int) -> Set (Set Int)
mezcla _ [] = []
mezcla n (x:xs) = (agregar n x) : (mezcla n xs)

partes :: Int -> Set (Set Int)
partes 0 = [[]]
partes n = unionS (mezcla n (partes (n-1))) (partes (n-1))
