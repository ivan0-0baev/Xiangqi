{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GamePieces
    (
        Player (Red, Black),
        PieceType (General, Advisor, Elephant, Horse, Rook, Cannon, Soldier),
        Piece(..),
        InterimPiece
    )
where

data PieceType = General | Advisor | Elephant | Horse | Rook | Cannon | Soldier deriving (Show, Read, Eq, Ord)
data Player = Red | Black deriving (Eq, Ord, Show)
data Piece = Piece { player :: Player, piecetype :: PieceType, pos :: Pos} deriving (Show, Eq)

type Pos = (Int,Int)
type InterimPiece = Maybe Piece