doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

fac :: Integer -> Integer
fac 0 = 1
fac n = fac2 (n , 1)
fac2 (0, acc) = acc
fac2 (n , acc) = fac2 (n-1, acc*n)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "Under"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Fat"
    where bmi = weight / height ^ 2

man = do putStrLn $ fac 30
