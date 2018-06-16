{- |
Module      : NQueens
Description : This module is for solving the N Queens problem in Haskell
Author      : Zach LeBlanc

This module is to solve the N Queens problem in Haskell. It includes functions
to find if you can place a queen in a certain location on the board and the
solution to the N Queens problem.
-}

module NQueens
    ( validQueenLoc
    , solveNQueens
    )
where

import           Board

-- | vaildRow board rowIndex
-- | This function takes a board and row index as parameters and returns true
-- | if this location in the row has no other queens in it. Assuming that a
-- | queen is represented by a 1 in the board.
validRow :: Board       -- ^ board
         -> Int         -- ^ rowIndex
         -> Bool        -- ^ true if a queen can be placed in this row
validRow board rowIndex = sum (getRow board rowIndex) <= 0

-- | validCol board colIndex
-- | This function takes a board and column index as parameters and returns 
-- | true if this location in the column has no other queens in it. Assuming
-- | that  queen is represented by a 1 in the board.
validCol :: Board       -- ^ board
         -> Int         -- ^ colIndex
         -> Bool        -- ^ true if a queen can be placed in this column
validCol board colIndex = sum (getCol board colIndex) <= 0

-- | validUpperDiags board rowIndex colIndex
-- | This function takes a board, row index, and column index as parameters and
-- | returns true if this location in the diags has no other queens in it.
-- | Assuming that a queen is represented by a 1 in the board.
validUpperDiags :: Board-- ^ board 
                -> Int  -- ^ rowIndex
                -> Int  -- ^ colIndex
                -> Bool -- ^ true if a queen can be plaved in this diag
validUpperDiags board rowIndex colIndex =
    sum
            (  upperLeftDiag board rowIndex colIndex
            ++ upperRightDiag board rowIndex colIndex
            )
        <= 0

-- | validQueenLoc board rowIndex colIndex
-- | This function takes a board, row index, and column index as parameters and
-- | returns true if this location is a valid to place a queen in it. Meaning
-- | that there are no other queens in the row, column or diagonals. Assumming
-- | that queens are represented by a 1.
validQueenLoc :: Board  -- ^ board
              -> Int    -- ^ rowIndex
              -> Int    -- ^ colIndex
              -> Bool   -- ^ returns true if a queen can be placed
validQueenLoc board rowIndex colIndex =
    validCol board colIndex
        && validRow board rowIndex
        && validUpperDiags board rowIndex colIndex

-- | solveNQueens N
-- | This function solves the N Queens problem and returns it as a board.
solveNQueens :: Int     -- ^ N size of the board N x N
             -> Board   -- ^ board of solved N queens problem
solveNQueens n = placeQueen (mkBoard n) 1 1
  where
    -- | placeQueen board rowIndex colIndex
    -- | This function is a helper function for solve N Queens. It recursively
    -- | attempts to place the queens so that they cannot attack each other.
    placeQueen board rowIndex colIndex
        | colIndex > n = [[]]
        | rowIndex > n = board
        | validQueenLoc board rowIndex colIndex
        = let
              b = placeQueen (setValue board rowIndex colIndex 1)
                             (rowIndex + 1)
                             1
          in if b == [[]] then placeQueen board rowIndex (colIndex + 1) else b
        | otherwise = placeQueen board rowIndex (colIndex + 1)
