import Data.Maybe
import System.Environment 
import System.Exit
import System.Random

maxNum = 100

main :: IO()
main = do
    args <- getArgs
    verifyArgsOrQuit args
    showSeed seed
    playGame $ getRandomGen seed
    putStrLn "Game over"

-- create random generator with seed value
getRandomGen :: Int -> StdGen
getRandomGen seed = mkStdGen seed

getSeed :: [String] -> IO Int
getSeed [] = getRandomSeed
getSeed (x:_) = return $ read x

getRandomSeed :: IO Int
getRandomSeed = do
    randomSrc <- getStdGen
    return $ fst $ System.Random.random randomSrc

playGame :: StdGen -> IO ()
playGame randomGen = do
    putStrLn $ "\nGuess the number: "
    let (rawTargetNum, nextGen) = next randomGen
    let target = rawTargetNum `mod` maxNum
    guessFor target 0
    showAnswer target
    again <- playAgain
    if again
        then playGame nextGen
        else quitGame

guessFor :: Int -> Int -> IO ()
guessFor target attempts = do
    putStr "Current guess? "
    guess <- getNum "\nCurrent guess? "
    if target == guess
        then guessCorrect $ attempts + 1
        else guessWrong target attempts guess

guessCorrect :: Int -> IO ()
guessCorrect numTries = do
    putStrLn $ "You won in " ++ show numTries ++ " guesses."

guessWrong :: Int -> Int -> Int -> IO ()
guessWrong target attempts guess = do
    if target > guess
        then putStrLn "Too Low"
        else putStrLn "Too High"

    guessFor target $ attempts + 1
