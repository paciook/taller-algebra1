factorial 0 = 1
factorial n = n * factorial (n-1)

combinatorio n m = div (factorial n) (factorial m * factorial (n-m))

sumaCombinatorio n k | n == k = 1
                     | otherwise = combinatorio k n + sumaCombinatorio (n+1) k
