type Set a = [a]

insertarOrdenado :: (Ord a) => a -> Set a -> Set a
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

union :: (Eq a, Ord a) => Set a -> Set a -> Set a
union [] l = l
union (x:xs) l = agregar x (union xs l)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion (x:xs) l | elem x l = agregar x (interseccion xs l)
                      | otherwise = interseccion xs l

mezcla :: (Ord a, Eq a) => a -> Set (Set a) -> Set (Set a)
mezcla _ [] = []
mezcla n (x:xs) = (agregar n x) : (mezcla n xs)

partes :: (Num a,Eq a, Ord a) => a -> Set (Set a)
partes 0 = [[]]
partes n = union (mezcla n (partes (n-1))) (partes (n-1))

parear :: (Ord a, Eq a) => a -> Set a -> Set (Set a)
parear _ [] = []
parear n (x:xs) = agregar [n,x] (parear n xs)

productoCartesiano :: (Ord a, Eq a) => Set a -> Set a -> Set (Set a)
productoCartesiano [] _ = []
productoCartesiano _ [] = []
productoCartesiano (x:xs) l = union (parear x l) (productoCartesiano xs l)
