import Data.Char

coprime :: Int -> Bool
coprime n = gcd n 26 == 1

inverse :: Int -> Int
inverse n = head [x | x <- [1..26], x * n `mod` 26 == 1]

affine :: Int -> Int -> Int -> Int
affine a b c = (a * c + b) `mod` 26

deaffine :: Int -> Int -> Int -> Int
deaffine a b c = inverse a * (c - b + 26) `mod` 26

encrypt :: Int -> Int -> Char -> Char
encrypt a b c
    | isAsciiLower c = chr $ affine a b (ord c - ord 'a') + ord 'a'
    | otherwise = c

decrypt :: Int -> Int -> Char -> Char
decrypt a b c
    | isAsciiLower c = chr $ deaffine a b (ord c - ord 'a') + ord 'a'
    | otherwise = c

encryptString :: Int -> Int -> String -> String
encryptString a b cs = [encrypt a b c | c <- cs]

decryptString :: Int -> Int -> String -> String
decryptString a b cs = [decrypt a b c | c <- cs]

forcaBruta :: String -> [String]
forcaBruta cs = [encryptString a b cs | a <- [n | n <- [1..26], coprime n],
                                        b <- [1.. 26]]

caesarString :: String -> String
caesarString cs = [caesar c | c <- cs]
    where caesar = encrypt 1 3

decaesarString :: String -> String
decaesarString cs = [decaesar c | c <- cs]
    where decaesar = decrypt 1 3

main :: IO()
main = do
    putStrLn $ encryptString 7 9 "Maurizio Mendes da Silva Bellinfante Crocci"
    putStrLn $ decryptString 7 9 "Mjtyncnd Mlwelf ej Sniaj Bliinwsjwml Cydxxn"
    
