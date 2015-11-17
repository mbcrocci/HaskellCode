import System.IO
import System.Environment

main :: IO()
main = do
    fileName <- getArgs
    handle <- openFile (head fileName) ReadMode
    contents <- hGetContents handle

    writeFile "new.txt" $ unlines $ map (unwords . quicksort .words) (lines contents)

    hClose handle


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
        let smaller = quicksort [a | a <- xs, a <= x]
            bigger  = quicksort [a | a <- xs, a > x]
        in smaller ++ [x] ++ bigger
