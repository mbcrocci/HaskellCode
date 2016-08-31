main :: IO ()
main = do
    putStrLn "--- MOLHOS ---"
    mapM_ print  $ combine 5 ["pitta", "samurai", "mostarda", "ketchup", "maionese"]

combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where
      combinations' _ _ [] = []
      combinations' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys

-- every combination from 1 to n
combine :: Int -> [a] -> [[[a]]]
combine n xs = map (`combinations` xs) [1..n]
