module Main where

doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x > 100 then x else x*2

boomBangs :: (Integral a) => [a] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

fac :: Integer -> Integer
fac 0 = 1
fac n = fac2 n 1
    where
        fac2 0 acc = acc
        fac2 x acc = fac2 (x-1) (acc*x)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "Under"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Fat"
    where bmi = weight / (height * height)

bmiTell _ _ = ""


main :: IO ()
main =  print $ show $ fac 30
