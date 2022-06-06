type Polinomio = [Float]
type Monomio = (Float, Int)

crearPolinomio :: [Float] -> Polinomio
crearPolinomio (0:xs) = crearPolinomio xs
crearPolinomio l = l

grado :: Polinomio -> Int
grado [] = undefined
grado [x] = 0
grado (x:xs) = 1 + grado xs

evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (x:xs) a = x * (a ^ (grado (x:xs))) + evaluar xs a

productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (_,n) [] = replicate n 0 {- Broken... esto no es un polinomio. -}
productoPorMonomio (a,n) (x:xs) = (a * x) : (productoPorMonomio (a,n) xs)

suma :: Polinomio -> Polinomio -> Polinomio
suma [] p2 = p2
suma p1 [] = p1
suma p1 p2 = (suma (init p1) (init p2)) ++ [(last p1) + (last p2)] {- Podemos usar init y last? -}

{- Si no podemos, tenemos esta funcion -}
sumaCorrecta :: Polinomio -> Polinomio -> Polinomio
sumaCorrecta [] p2 = p2
sumaCorrecta p1 [] = p1
sumaCorrecta p1 p2 | grado p1 == grado p2 = (x + y) : subsuma
                   | grado p1 > grado p2 = x : sumaCorrecta (tail p1) p2
                   | otherwise = sumaCorrecta p2 p1
                   where x = head p1
                         y = head p2
                         subsuma = sumaCorrecta (tail p1) (tail p2)

producto :: Polinomio -> Polinomio -> Polinomio
producto [] p = []
producto (x:xs) p = suma{-Correcta-} (productoPorMonomio (x,grado (x:xs)) p) (producto xs p)

evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple [] p q = []
evaluacionMultiple (i:is) p q | mod (length {-largo-} is) 2 == 0 = evaluar p i : siguiente {- Podemos usar length? Que es Data.Foldable? -}
                              | otherwise = (evaluar q i) : siguiente
                              where siguiente = evaluacionMultiple is p q

{- Si no podemos, tenemos esta funcion -}
largo :: [Float] -> Int
largo [] = 0
largo (_:xs) = 1 + largo xs
