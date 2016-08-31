main :: IO ()
main = do
    let board = initBoard 6 7 "x"
    putStr $ boardString board
    putStrLn "GOING TO EAT"
    putStr $ boardString $ eat board 3 4
    putStrLn "ALREADY ATE"

data Piece = Piece String deriving Show
type Board = [[Piece]]

initRow :: Int -> String -> [Piece]
initRow 0 _ = []
initRow size c = Piece c : initRow (size-1) c

initBoard :: Int -> Int -> String -> Board
initBoard _ 0 _ = [[]]
initBoard r c b = initRow r b : initBoard r (c-1) b

pieceString :: Piece -> String
pieceString (Piece c) = c

rowString :: [Piece] -> String
rowString [] = ""
rowString (r:rs) = pieceString r  ++ " " ++ rowString rs

boardString :: Board -> String
boardString [] = ""
boardString (b:bs) = rowString b ++ "\n" ++ boardString bs

-- TODO(mbcrocci): probably doesnt work
eatRow :: [Piece] -> Int -> Int -> [Piece]
eatRow [] _ _ = []
eatRow (_:rs) end acc = Piece " " : eatRow rs end (acc+1)

-- TODO(mbcrocci): Doesn't work, need to change
eat :: Board -> Int -> Int -> Board
eat [] _ _ = []
eat board row col = eatHelp board row 0 col
    where
        eatHelp :: Board -> Int -> Int -> Int -> Board
        eatHelp [] _ _ _ = []
        eatHelp (b:bs) r rowAcc c
            | row >= rowAcc = []
            | otherwise = eatRow b c 0 : eatHelp bs r (rowAcc+1) c
