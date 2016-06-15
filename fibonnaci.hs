-- Lazy-evaluated infinite generator of the fibonacci sequence
fibGenerator :: [Integer]
fibGenerator = 1 : 1 : [a + b | (a, b) <- zip fibGenerator (tail fibGenerator)]

-- Standard recursive implementation
fibRec :: (Show a, Eq a, Num a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)

-- Standar recusive implementation with a TAIL optimization
fibTail :: (Eq a, Num a, Show a) => a -> a
fibTail a = fibHelp a 0 1
    where
        fibHelp 0 acc _ = acc
        fibHelp n acc b = fibHelp (n-1) b (acc+b)

main :: IO()
main = do
    print $ show $ last $ take 300 fibGenerator
    print $ show (fibRec 30 :: Integer)
    print $ show (fibTail 300000:: Integer)
