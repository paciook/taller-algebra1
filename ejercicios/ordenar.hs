insertarOrdenado :: Int -> [Int] -> [Int]
insertarOrdenado n [] = [n]
insertarOrdenado n (x:xs) | n <= x = n : x : xs
                          | otherwise = x : insertarOrdenado n xs

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = insertarOrdenado x (ordenar xs)

