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
productoPorMonomio (_,n) [] = replicate n 0
productoPorMonomio (a,n) (x:xs) = (a * x) : (productoPorMonomio (a,n) xs)

suma :: Polinomio -> Polinomio -> Polinomio
suma [] p2 = p2
suma p1 [] = p1
suma p1 p2 = (suma (init p1) (init p2)) ++ [(last p1) + (last p2)]

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
producto (x:xs) p = suma (productoPorMonomio (x,grado (x:xs)) p) (producto xs p)

largo :: [Float] -> Int
largo [] = 0
largo (x:xs) = 1 + largo xs

evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple [] p q = []
evaluacionMultiple (i:is) p q | mod (largo is) 2 == 0 = evaluar p i : siguiente
                              | otherwise = (evaluar q i) : siguiente
                              where siguiente = evaluacionMultiple is p q
