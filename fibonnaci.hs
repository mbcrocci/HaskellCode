fibGenerator :: [Integer]
fibGenerator = 1 : 1 : [a + b | (a, b) <- zip fibGenerator (tail fibGenerator)]

fibRec :: (Show a, Eq a, Num a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)

fibTail :: (Eq a, Num a, Show a) => a -> a
fibTail a = fibTailRec a 0 1

fibTailRec :: (Show a, Eq a, Num a) => a -> a -> a -> a
fibTailRec 0 a _ = a
fibTailRec n a b = fibTailRec (n-1) b (a+b)

main :: IO()
main = do
    print $ show $ last $ take 30 fibGenerator
    print $ show (fibRec 30 :: Integer)
    print $ show (fibTail 30 :: Integer)

    -- print $ show (fibGenerator !! 20 :: Integer) nao funciona correctamente
