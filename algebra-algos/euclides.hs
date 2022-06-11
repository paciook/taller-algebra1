mcd a 0 = abs a
mcd a b | a < b = mcd b a
        | otherwise = mcd b (mod a b)
