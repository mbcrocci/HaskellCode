import System.IO

main :: IO()
main = do
    handle <- openFile "ips.txt" ReadMode
    contents <- hGetContents handle

    -- lista de ips
    let ips = lines contents

    hClose handle
