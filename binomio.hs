fac :: (Integral a) => a -> a
fac 1 = 1
fac n = n * fac (n-1)

comb :: (Fractional a) => a -> a -> a
comb n p
        | n < p = 1
        | otherwise = fac n / (fac p * fac (n-p))

binomio :: (Num a) => a -> a -> a -> a
binomio a b c = (a + b) ^ c
