{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Main
where

-- Diese Datei ist nicht Teil der Abgabe, sondern
-- nur als Hilfe fuer das Erstellen eines ausfuehrbaren Bots

-- DIESES MODUL NICHT ÄNDERN!!!

import System.Environment
import qualified Data.List as List

import XiangqiBot
import GameBoard
import GamePieces
import InterpretFen
import MoveLogic
import UpdateBoard
import System.Exit (exitSuccess)



main :: IO ()
main = do
  --displayStart
  putStrLn "Type [start game] to begin"
  putStrLn "Type [end]        to end game"
  args <- getLine
  if null args then return ()
  else if args == "start game"
    then
      game startFen
    else return ()


-- Currently doesn't check if entered String is not a move
game :: String -> IO()
game fen = do
    display fen
    move <- getLine
    if null move
      then putStrLn "No Move Entered"
      else if move == "end"
          then
              exitSuccess
          else
            putStrLn "Move Entered"
    if null move
      then game fen
      else putStrLn ""

    let
      board = concat (splitOn '\n' (takeBoardString fen))
      player = playerTurn fen
      newBoardInter = checkMove move board fen
      msg = checkIll move board fen
      
      newBoard = takeBoardString newBoardInter
      newBoardFen = boardToFen newBoard player
      
      pl = if player == Red then "Next Player: Black"
                            else "Next Player: Red"
      --newBoard = setMove move board
      --if newBoard == fen 
      --  then putStrLn "Illegal Move"
      --  else putStrLn ""

    --print "Illegal Move!"
    
    putStrLn msg
    if msg == "Illegal Move" then putStrLn ("")
    else putStrLn pl
    if msg == "Illegal Move" then game fen
    else game newBoardFen


-- Checks Legality of Move
-- If Legal returns updated Board
-- Else Returns old Board and Warns that Move Is Illegal
checkIll :: String -> String -> String -> String
checkIll move board fen =
  let
    legalMoves = listMoves fen
    legal = checkLegal move legalMoves
    msg = if legal
                  then "Move Approved"
                  else "Illegal Move"
  in
    msg

checkMove :: String -> String -> String -> String
checkMove move board fen = newBoard
  where
    legalMoves = listMoves fen
    legal = checkLegal move legalMoves
    newBoard = if legal
                  then setMove move board
                  else board

    --putStrLn "Illegal Move"

    --if legal then putStrLn "..." else putStrLn "Illegal Move"


printB :: String -> IO()
printB = putStrLn


startBoard = concat (splitOn '\n' (takeBoardString "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"))
startFen = "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"