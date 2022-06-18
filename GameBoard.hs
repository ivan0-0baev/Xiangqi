{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use list literal" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GameBoard 
    (
        Board(..),
        displayStart,
        display,
        actuallBoard
    ) 
    where

import Data.Array
import GamePieces
import InterpretFen


type Board = Array Pos InterimPiece
data BoardState = BoardState {boardState :: Board, turnOf :: Player}

--type InterimPiece = Maybe Piece
type Pos = (Int,Int)

-- returns Board as Array From input String
actuallBoard :: String -> Board
actuallBoard str = listArray ((0,0),(9,8)) $ fenToBoard str

startBoard = actuallBoard "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"

displayStart :: IO()
displayStart = putStr (
    "   a  b  c  d  e  f  g  h  i \n\
    \9 [r][h][e](a)(g)(a)[e][h][r]\n\
    \8 [ ][ ][ ]( )( )( )[ ][ ][ ]\n\
    \7 [ ][c][ ]( )( )( )[ ][c][ ]\n\
    \6 [s][ ][s][ ][s][ ][s][ ][s]\n\
    \5 [ ][ ][ ][ ][ ][ ][ ][ ][ ]\n\
    \  xxxxxxxxxxxxxxxxxxxxxxxxxxx\n\
    \4 [ ][ ][ ][ ][ ][ ][ ][ ][ ]\n\
    \3 [S][ ][S][ ][S][ ][S][ ][S]\n\
    \2 [ ][C][ ]( )( )( )[ ][C][ ]\n\
    \1 [ ][ ][ ]( )( )( )[ ][ ][ ]\n\
    \0 [R][H][E](A)(G)(A)[E][H][R]\n"
    )


--add Brackets 
--addBracket :: Char -> String
addBracket :: Char -> [Char]
addBracket '1' = '[':' ':']':[]
addBracket c = '[':c:']':[]

changeBrackets :: Char -> [Char]
changeBrackets '[' = '(':[]
changeBrackets ']' = ')':[]
changeBrackets c = c:[]

-- Take Fen String And print Board
display :: String -> IO()
display fen = 
    let
        mod = concat (splitOn '\n' (takeBoardString fen))
        modList = concatMap addBracket mod
        row0 = take 27 modList
        row00 = take 9 row0 ++ concatMap changeBrackets (drop 9 (take 18 row0)) ++ drop 18 row0
        modList1 = drop 27 modList
        row1 = take 27 modList1
        row11 = take 9 row1 ++ concatMap changeBrackets (drop 9 (take 18 row1)) ++ drop 18 row1
        modList2 = drop 27 modList1
        row2 = take 27 modList2
        row22 = take 9 row2 ++ concatMap changeBrackets (drop 9 (take 18 row2)) ++ drop 18 row2
        modList3 = drop 27 modList2
        row3 = take 27 modList3
        modList4 = drop 27 modList3
        row4 = take 27 modList4
        modList5 = drop 27 modList4
        row5 = take 27 modList5
        modList6 = drop 27 modList5
        row6 = take 27 modList6
        modList7 = drop 27 modList6
        row7 = take 27 modList7
        row77 = take 9 row7 ++ concatMap changeBrackets (drop 9 (take 18 row7)) ++ drop 18 row7
        modList8 = drop 27 modList7
        row8 = take 27 modList8
        row88 = take 9 row8 ++ concatMap changeBrackets (drop 9 (take 18 row8)) ++ drop 18 row8
        modList9 = drop 27 modList8
        row9 = take 27 modList9
        row99 = take 9 row9 ++ concatMap changeBrackets (drop 9 (take 18 row9)) ++ drop 18 row9

        colNames = "   a  b  c  d  e  f  g  h  i \n"

    in
        putStr( 
            colNames ++
            "9 " ++ row00 ++ "\n" ++ 
            "8 " ++ row11 ++ "\n" ++
            "7 " ++ row22 ++ "\n" ++
            "6 " ++ row3 ++ "\n" ++
            "5 " ++ row4 ++ "\n" ++
            "  " ++ "xxxxxxxxxxxxxxxxxxxxxxxxxxx" ++ "\n" ++
            "4 " ++ row5 ++ "\n" ++
            "3 " ++ row6 ++ "\n" ++
            "2 " ++ row77 ++ "\n" ++
            "1 " ++ row88 ++ "\n" ++
            "0 " ++ row99 ++
            "\n"
            )



{- Game Board overlay
  a b c d e f g h i
0 [][][]()()()[][][]
1 [][][]()()()[][][]
2 [][][]()()()[][][]
3 [][][][][][][][][]
4 [][][][][][][][][]
  xxxxxxxxxxxxxxxxxx
5 [][][][][][][][][]
6 [][][][][][][][][]
7 [][][]()()()[][][]
8 [][][]()()()[][][]
9 [][][]()()()[][][]
-}