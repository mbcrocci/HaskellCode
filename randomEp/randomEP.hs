#!/usr/local/bin/runhaskell

import System.Environment
import System.Random
import System.IO.Unsafe

main :: IO ()
main = do
    args <- getArgs
    let seasons = getSeasons args
    let season = randomNumber (length seasons)
    let episode = randomNumber (seasons !! (season - 1))
    putStrLn $ "You should watch:\n\t[SEASON]: " ++
                show season ++ "\n\t[EPISODE]: " ++ show episode


getSeasons :: [String] -> [Int]
getSeasons [] = []
getSeasons xs = map (\x -> read x :: Int) xs

randomNumber :: (Random a, Num a) => a -> a
randomNumber n = unsafePerformIO $ randomRIO (1, n)
