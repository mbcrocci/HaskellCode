import System.Directory

main :: IO ()
main = mapM_ printDir ["Movies", "TV Shows", "untitled folder"]

printDir :: String -> IO()
printDir folderName = do
    let path = "/Volumes/PunkEHDD/" ++ folderName
    setCurrentDirectory path
    dir <- getCurrentDirectory
    list <- listDirectory dir
    putStr $  "\n ------------ " ++ folderName ++ " ------------\n\n"
    mapM_ putStrLn list
