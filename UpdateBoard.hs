{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module UpdateBoard 
    (
        setMove
    )
    where

import InterpretFen
import GameBoard

-- Gives Coordinates in required format
showCoords :: String -> Int 
showCoords coord = 
    let 
        col = head coord
        row = tail coord
        r = showRow row
        c = showCol col
    in
        c + r*9

showRow :: String -> Int 
showRow "9" = 0
showRow "8" = 1
showRow "7" = 2
showRow "6" = 3
showRow "5" = 4
showRow "4" = 5
showRow "3" = 6
showRow "2" = 7
showRow "1" = 8
showRow "0" = 9

showCol :: Char -> Int
showCol 'a' = 0
showCol 'b' = 1
showCol 'c' = 2
showCol 'd' = 3 
showCol 'e' = 4
showCol 'f' = 5
showCol 'g' = 6
showCol 'h' = 7
showCol 'i' = 8

replace :: Int -> Char -> String -> String
replace indx repl fen = [if j == indx then repl else c | (j, c) <- zip [0..] fen]

-- Player Gives Move (Start-End) and Updates Fen String. Exmpl: a0-a2
setMove :: String -> String -> String
setMove x fen =
    let
        str = splitOn '-' x
        start = head str
        end = str!!1
        moveCoordsStart = showCoords start
        moveCoordsEnd = showCoords end
        piece = fen!!moveCoordsStart
        temp = replace moveCoordsStart '1' fen
        temp1 = replace moveCoordsEnd piece temp
    in
        temp1
