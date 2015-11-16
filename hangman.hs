import System.IO
import System.Random

main :: IO ()
main = do
    fileHandler <- openFile "enable1.txt" ReadMode
    contents <- hGetContents fileHandler
    gen <- getStdGen
    let fileWords = map init (lines contents)
        (n, _)    = randomR (0, length fileWords - 1) gen :: (Int, StdGen)
        word      = fileWords !! n

    play word (map (const '_') word) 6
    hClose fileHandler

play :: (Show t, Num t, Eq t) => String -> String -> t -> IO ()
play word known guesses
    | word == known = do
        putStrLn known
        putStrLn "You win!"

    | guesses == 0 = do
        putStrLn known
        putStrLn ("You Lose. The word was " ++ word ++ ".")

    | otherwise = do
        putStrLn known
        putStrLn ("You have " ++ show guesses ++ " guesses left.")
        line <- getLine
        let (newKnown, newGuesses) = handle (head line) word known guesses
        play word newKnown newGuesses

handle :: (Num t, Eq c) => c -> [c] -> [c] -> t -> ([c], t)
handle letter word known guesses
    | letter `elem` word = (zipWith (\w k -> if w == letter then w else k) word known, guesses)
    | otherwise          = (known, guesses - 1)
