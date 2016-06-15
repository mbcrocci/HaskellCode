import Test.QuickCheck

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
        let smaller = quicksort [a | a <- xs, a <= x]
            bigger  = quicksort [a | a <- xs, a > x]
        in smaller ++ [x] ++ bigger

prop_test_quicksort :: Ord a => [a] -> Bool
prop_test_quicksort xs = quicksort (quicksort xs) == quicksort xs

quickcheckInt = quickCheck (prop_test_quicksort :: [Integer] -> Bool)
quickcheckDouble = quickCheck (prop_test_quicksort :: [Double] -> Bool)

verbcheckInt = verboseCheck (prop_test_quicksort :: [Integer] -> Bool)
verbcheckDouble = verboseCheck (prop_test_quicksort :: [Double] -> Bool)