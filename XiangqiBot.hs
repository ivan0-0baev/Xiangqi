{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}
module XiangqiBot
    (
        listMoves,
        checkLegal
    )
    where

import Data.Char ()
import Data.Array
import Data.Maybe
import System.Win32 (COORD(x), BOOL)

import GamePieces
import GameBoard
import MoveLogic
import InterpretFen


--getMove :: String -> String
--getMove str = tail (take 6 (listMoves str))

getMoveAr:: [String] -> String
getMoveAr str = str!!1

listMoves :: String -> String
listMoves str =
    let
        b = actuallBoard str
        s = playerTurn str
        ans = getAllMoves b s
        inter@(piece, toPos) = unzip ans
        starts = map pos piece
        twoPos = zip starts toPos
        fromString = map showCoords starts
        toString = map showCoords toPos
        strTupls = zip fromString toString
        listOfStr = map mergeTupleStr strTupls
        listOfStrFinal = init (concat listOfStr)
    in
--        "[" ++ listOfStrFinal ++ "]"
        listOfStrFinal


-- returns All Board Positions: (0,0) -> (9,8)
boardPositions = [(x,y) | x<-[0..9], y<-[0..8]]

-- returns list of all posible moves for piece
possibleMoves board piece = filter (isLegal board piece) boardPositions

-- list of all possible moves for current player
getAllMoves b s = concatMap (\x-> zip (repeat x) $ possibleMoves b x) $ getPlayer b s

getPlayer b s = filter (\x-> player x == s) $ getPieces b

getPieces :: Board -> [Piece]
getPieces = catMaybes.elems

-- Start Board for Testing
startBoard = actuallBoard "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"

-- Gives Coordinates in required format
showCoords :: (Int,Int) -> String
showCoords cords@(row, col) =
    let
        r = showRow row
        c = showCol col
    in
        c ++ r

showRow :: Int -> String
showRow 0 = "9"
showRow 1 = "8"
showRow 2 = "7"
showRow 3 = "6"
showRow 4 = "5"
showRow 5 = "4"
showRow 6 = "3"
showRow 7 = "2"
showRow 8 = "1"
showRow 9 = "0"

showCol :: Int -> String
showCol 0 = "a"
showCol 1 = "b"
showCol 2 = "c"
showCol 3 = "d"
showCol 4 = "e"
showCol 5 = "f"
showCol 6 = "g"
showCol 7 = "h"
showCol 8 = "i"



mergeTupleStr :: (String,String) -> String
mergeTupleStr str@(a, b) =  a ++ "-" ++ b ++ ","


checkLegal :: String -> String -> BOOL
checkLegal move list = move `elem` (splitOn ',' list)

{-
getList :: String -> String
getList str = 
    let 
        b = actuallBoard str
        s = playerTurn str
        ans = getAllMoves b s 
        inter@(piece, toPos) = unzip ans
        starts = map pos piece 
        twoPos = zip starts toPos
        fromString = map showCoords starts
        toString = map showCoords toPos  
        strTupls = zip fromString toString
        listOfStr = map mergeTupleStr strTupls 
    
        listOfStrFinal = init (concat listOfStr) 
    in
        "[" ++ listOfStrFinal ++ "]"
-}
