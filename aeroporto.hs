module Main where

import System.Exit
import System.IO.Unsafe
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad(liftM)

main :: IO ()
main = menu []

menu :: FlightList -> IO ()
menu flightList = do
    putStrLn "1 - New Flight"
    putStrLn "2 - Show all Flights"
    putStrLn "3 - Show next 5 Flights"
    putStrLn "4 - Udapte flight table"
    putStrLn "6 - Exit"

    putStr " Enter Option: "
    opt <- getLine
    let iopt = read opt

    if iopt == 1 then
        menu $ addFlight flightList newFlight

    else if iopt == 2 then do
            showAllFlights flightList
            menu flightList

    else if iopt == 3 then do
            showNext5Flights flightList
            menu flightList

    else if iopt == 4 then
         menu $ updateList flightList

    else if iopt == 6 then
        exitSuccess

    else do
        putStrLn "\nNot valid\n"
        menu flightList


type FlightList = [Flight]
type Number     = String
type Company    = String
type Desteny    = String
type Hour       = Int

data Flight = Flight Number Company Desteny --Hour
                deriving (Show, Eq, Ord)


prompt :: (Read a) => String -> IO a
prompt s = liftM read  (putStr s >> getLine)

newFlight :: Flight
{-# NOINLINE newFlight #-}
newFlight = unsafePerformIO newFlight'

newFlight' :: IO Flight
newFlight' =  do
    putStr "Enter flight number: "
    number <- getLine

    putStr "Enter flight company: "
    company <- getLine

    putStr "Enter flight desteny: "
    desteny <- getLine

    --hour <- prompt "Enter flights hour: "

    return $ Flight number company desteny --hour

addFlight :: FlightList -> Flight -> FlightList
addFlight [] f     = [f]
addFlight fl f    = fl ++ [f]

showAllFlights :: FlightList -> IO ()
showAllFlights [] = putStrLn "Empty List"
showAllFlights (x:xs)  = do
                        print x
                        showAllFlights xs

showNext5Flights :: FlightList -> IO ()
showNext5Flights [] = putStrLn "Empty List"
showNext5Flights f = showAllFlights (take 5 f)

-- updateList :: sees if flight already departed, and if so, removes it
updateList :: FlightList -> FlightList
updateList []     = []
updateList (x:xs)
        | x < undefined = removeItem x xs
        | otherwise = updateList xs


-- removeItem :: helper function that actually removes the flight from the list
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys)
        | x == y = removeItem x ys
        | otherwise = y : removeItem x ys

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = Control.Monad.liftM (toGregorian . utctDay) getCurrentTime
