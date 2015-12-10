fac :: (Show a, Integral a) => a -> a
fac 1 = 1
fac n = fac2 n 1
    where
        fac2 0 acc = acc
        fac2 x acc = fac2 (x-1) (acc*x)


comb :: (Show a, Fractional a, Ord a, Integral a) => a -> a -> a
comb n p
        | n < p = 1
        | otherwise = fac n / (fac p * fac (n-p))

binomio :: (Integral a, Num a) => a -> a -> a -> a
binomio a b c = (a + b) ^ c

main :: IO ()
main = print $ show (fac 30 :: Integer)
