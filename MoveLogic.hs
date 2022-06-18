{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Redundant bracket" #-}
module MoveLogic
    (
        isLegal,

    )
where

import GamePieces
import GameBoard
import Data.Maybe
import Data.Array

type Pos = (Int, Int)

-- Move Logic
isLegal :: Board -> Piece -> Pos -> Bool
isLegal board piece@(Piece s t (oldX,oldY)) newPos@(newX,newY)
  =
  inBoundsRow newX && inBoundsCol newY -- check if In Bounds
  &&
    dX+dY/=0 -- No staying in the same position
  &&
  case t of -- move Logic
    Soldier -> (dY==0 && oldX`pm`1==newX) || case s of
                Black -> dY==1 && oldX >= 5 && dX==0
                Red -> dY==1 && oldX <= 4 && dX==0
                
    Horse -> (dX, dY) `elem`[(1,2),(2,1)] && case compare dX dY of
                LT -> case compare newY oldY of -- horse move horizontally
                    LT -> if inBoundsCol (newY+1) then isNothing (board!(oldX,newY+1)) else True
                    GT -> if inBoundsCol (newY-1) then isNothing (board!(oldX,newY-1)) else True
                GT -> case compare newX oldX of -- horse move vertically 
                    LT -> if inBoundsRow (oldX-1) then isNothing (board!(oldX-1,oldY)) else True
                    GT -> if inBoundsRow (oldX+1) then isNothing (board!(oldX+1,oldY)) else True

    Elephant -> dX==dY && dX==2 && dY==2 && case s of
                Black -> newX <= 4
                Red -> newX >= 5
    Rook -> dX==0 || dY==0
    Advisor -> dX==dY && dX==1 && dY==1 && case s of
                Black -> newX<=2 && newY>=3 && newY<=5
                Red -> newX>=7 && newY>=3 && newY<=5
    General -> ((dX==1 && dY==0)||(dX==0 && dY==1)) && generalEyeContact newPos s board && case s of
                Black -> newX<=2 && newY>=3 && newY<=5
                Red -> newX>=7 && newY>=3 && newY<=5
    Cannon -> (dX==0 || dY==0) -- && (length inbetweens <= 1 && )
  &&
    if t == Horse then True
    else
        (null inbetweens 
            || (((pos firstHit == newPos && t /= Cannon) ) && hitEnemy) || (t == Cannon && length inbetweens == 2 && hitEnemy))
                                                                    
  where
    pm = case s of Black->(+); Red->(-)
    inBoundsRow x =  x>=0 && x<=9
    inBoundsCol y = y>=0 && y<=8
    dX = abs $ newX-oldX
    dY = abs $ newY-oldY
    inbetweens = catMaybes $ tail [board!(x,y) | (x,y) <- ps]
        where ps = map (\s -> (oldX+(sr*s), oldY+(sc*s))) [0..(d)]
              d = distance (oldX, oldY) (newX, newY)
              (sr, sc) = ((newX-oldX) `div` d, (newY-oldY) `div` d)
    distance (c0, r0) (c1, r1) = max (abs $ c1-c0) (abs $ r1-r0)
    firstHit = head inbetweens
    hitEnemy =  case board!newPos of
      Nothing -> otherwise
      Just victim -> player victim /= s



      
getGeneral :: [Piece] -> [Piece]
getGeneral = filter (\x -> piecetype x == General  )

-- Take new Pos of General and checks if There is Eye Contact: Fasle => EyeContact ; True => Viable Move
generalEyeContact :: Pos -> Player -> Board -> Bool
generalEyeContact p@(r,c) pl b =
    let
        genPos = getGeneral (getPieces b)
        getOpositionG = case pl of
                        Red -> head genPos
                        Black -> last genPos
        opGenPos@(rx,cy) = pos getOpositionG
        cover = catMaybes [b!(x,y) | (x,y) <- ps ]
                where
                      ps = [(x,c) | x<- [sm+1..lg-1], y <- [c] ]
                      sm = min r rx
                      lg = max r rx

    in
        if length cover /= 0 || c /= cy then True else False


getPieces :: Board -> [Piece]
getPieces = catMaybes.elems

-- Take new Pos of Piece and checks if There is Eye Contact between Generals: Fasle => EyeContact ; True => Viable Move
abandonGeneral :: Piece -> Board -> Bool
abandonGeneral piece b =
    let
        genPos = getGeneral (getPieces b)
        getPosB@(rb,cb) = pos (head genPos)
        genPosR@(rr,cr) = pos (last genPos)

        cover = catMaybes [b!(x,y) | (x,y) <- ps ]
                where
                      ps = [(x,y) | x<- [sm+1..lg-1], y <- [cb] ]
                      sm = min rb rr
                      lg = max cb cr

    in
        if length cover /= 0 || cb /= cr then True 
        else if length cover == 1 && (piece `elem` cover) then False 
        else False