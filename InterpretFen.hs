{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module InterpretFen 
    (
        fenToBoard,
        playerTurn,
        takeBoardString,
        splitOn,
        boardToFen
    )
    where

import Data.Array
--import GameBoard
import GamePieces

type Interim = (String, Int)

-- >>> splitOn ',' "foo,foobar,foobarbaz,"
-- ergibt ["foo", "foobar", "foobarbaz", ""]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x ys = case break (==x) ys of
    (ys1, []) -> [ys1]
    (ys1, _:ys2) -> ys1 : splitOn x ys2

-- returns Index from Int
fromIndex :: Int -> (Int,Int)
fromIndex n = (n `quot` 9,n `mod` 9 )


-- Modify Strings fen to board
stringChange :: Char -> String
stringChange str
  | str == '9' = "111111111"
  | str == '8' = "11111111"
  | str == '7' = "1111111"
  | str == '6' = "111111"
  | str == '5' = "11111"
  | str == '4' = "1111"
  | str == '3' = "111"
  | str == '2' = "11"
  | str == '/' = "\n"
  | otherwise = [str]


-- Reads Chars and returns Game Piece
readPiece :: Interim -> Piece

readPiece ("R",n) = (Piece Red Rook $ fromIndex n)
readPiece ("G",n) = (Piece Red General $ fromIndex n)
readPiece ("A",n) = (Piece Red Advisor $ fromIndex n)
readPiece ("E",n) = (Piece Red Elephant $ fromIndex n)
readPiece ("H",n) = (Piece Red Horse $ fromIndex n)
readPiece ("C",n) = (Piece Red Cannon $ fromIndex n)
readPiece ("S",n) = (Piece Red Soldier $ fromIndex n)

readPiece ("r",n) = (Piece Black Rook $ fromIndex n)
readPiece ("g",n) = (Piece Black General $ fromIndex n)
readPiece ("a",n) = Piece Black Advisor $ fromIndex n
readPiece ("e",n) = (Piece Black Elephant $ fromIndex n)
readPiece ("h",n) = (Piece Black Horse $ fromIndex n)
readPiece ("c",n) = (Piece Black Cannon $ fromIndex n)
readPiece ("s",n) = (Piece Black Soldier $ fromIndex n)


-- Divide String into Characters
modify :: String -> String
modify listStrings = concat (map stringChange listStrings)

-- Takes String and Returns only Board Part
takeBoardString :: String ->  String
takeBoardString string = ( take 99 (modify string) )

-- Removes Empty Spaces in Fen String
createPiece :: Interim -> InterimPiece
createPiece ("1", _) = Nothing
createPiece (c, n) =  Just (readPiece (c, n))



-- Fen String To List of Maybe Game Pieces
fenToBoard :: String -> [InterimPiece]
fenToBoard str =
    let
        board = takeBoardString str
        boardStr = concat (splitOn '\n' board)
    in
        zipWith (curry createPiece) (fmap pure boardStr) [0..89]


-- Give player 
playerTurn :: String -> Player
playerTurn string
    | drop 100 (modify string) == "r" = Red
    | drop 100 (modify string) == "b" = Black


-- Takes Board-Like Fen string and transforms it to the required format
-- Exmpl -> 
-- "rheagaehr1111111111c11111c1s1s1s1s1s111111111111111111S1S1S1S1S1C11111C1111111111RHEAGAEHR"
-- "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"
boardToFen :: String -> Player -> String 
boardToFen board player = 
    let 
        row0 = take 9 board
        board0 = drop 9 board
        row1 = take 9 board0
        board1 = drop 9 board0
        row2 = take 9 board1
        board2 = drop 9 board1
        row3 = take 9 board2
        board3 = drop 9 board2
        row4 = take 9 board3
        board4 = drop 9 board3
        row5 = take 9 board4
        board5 = drop 9 board4
        row6 = take 9 board5
        board6 = drop 9 board5
        row7 = take 9 board6
        board7 = drop 9 board6
        row8 = take 9 board7
        board8 = drop 9 board7
        row9 = take 9 board8

        pl = if player == Red then "b"
                            else "r"

    in
        (row0 ++ "/" ++ row1 ++ "/" ++ row2 ++ "/" ++ row3 ++ "/" ++ row4 ++ "/" ++
        row5 ++ "/" ++ row6 ++ "/" ++ row7 ++ "/" ++ row8 ++ "/" ++ row9 ++ " " ++ pl)