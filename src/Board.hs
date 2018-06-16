{- |
Module      : Board
Description : This module is for representing a board in Haskell.
Author      : Zach LeBlanc, Aylce Brady

This module is to represent a board for the N Queens problem in Haskell. All of
the functions have to do with accessing elements in a board, setting elements
in a board and creating a board which is a 2d list.
-}

module Board
    ( Row
    , Column
    , Board
    , Diag
    , mkRow
    , mkBoard
    , mkTestBoard
    , getRow
    , getCol
    , upperLeftDiag
    , upperRightDiag
    , setValue
    , getCell
    , validIndex
    , validLoc
    )
where

-- | Row, Column, and Diag are renamed Interget lists for readability
type Row = [Integer]
type Column = [Integer]
type Diag = [Integer]
type Board = [Row] -- | Board is simply a list of rows or [[Integers]]

------------------------------------------------------
-- FUNCTIONS TO CREATE A BOARD
------------------------------------------------------

-- mkRow N
--   Creates an "empty" row of length N (actually a row of 0's).
mkRow :: Int            -- ^ N the length of the row
      -> Row            -- ^ list of zeros length of N
mkRow n = replicate n 0

-- mkBoard N
--   Creates an "empty" N x N board (actually filled with 0's).
mkBoard :: Int          -- ^ N the size of the 2d array
        -> Board        -- ^ 2d array filled with 0's of size N
mkBoard n = replicate n (mkRow n)

------------------------------------------------------
-- FUNCTION TO CREATE TEST BOARD
------------------------------------------------------

-- mkTestBoard N
--   Creates an N x N board whose 2-digit values encode their location:
--   the first digit is the row number (starting at 1) and the second is
--   the column number (starting at 1).  For example,
--     mkTestBoard 3
--   creates the following board:
--     [[11,12,13],[21,22,23],[31,32,33]]
mkTestBoard :: Integer  -- ^ N size of test board
            -> Board    -- ^ board of indexed locations
mkTestBoard n = [ [ x * 10 + y | y <- [1 .. n] ] | x <- [1 .. n] ]

-- ------------------------------------------------------
-- -- FUNCTIONS TO ACCESS ELEMENTS OF A BOARD
-- ------------------------------------------------------

-- getRow board rowIndex
-- Returns the row at index rowIndex.
getRow :: Board         -- ^ board
       -> Int           -- ^ rowIndex
       -> Row           -- ^ list of values from the row
getRow board rowNum = board !! (rowNum - 1)

-- getCol board colIndex
--   Returns the column at index colIndex.
getCol :: Board         -- ^ board
       -> Int           -- ^ colIndex
       -> Column        -- ^ list of values from the column
getCol board colNum =
    [ getCell board rowNum colNum | rowNum <- [1 .. (length board)] ]

-- upperLeftDiag board fromRow fromCol
-- Returns the upper left diag starting at fromRow, fromCol
upperLeftDiag :: Board  -- ^ board
              -> Int    -- ^ fromRow starting index of the row
              -> Int    -- ^ fromCol starting index of the col
              -> Diag   -- ^ list of values from the upper left diag
upperLeftDiag board fromRow fromCol =
    [ getCell board r (fromCol - (fromRow - r))
    | r <- [1 .. (fromRow - 1)]
    , validIndex board (fromCol - (fromRow - r))
    ]

-- upperRightDiag board fromRow fromCol
-- Returns the upper right diag starting at fromRow, fromCol
upperRightDiag :: Board -- ^ board
               -> Int   -- ^ fromRow starting index of the row
               -> Int   -- ^ fromCol starting index of the col
               -> Diag  -- ^ list of values from the upper right diag
upperRightDiag board fromRow fromCol =
    [ getCell board r (fromCol + (fromRow - r))
    | r <- [1 .. (fromRow - 1)]
    , validIndex board (fromCol + (fromRow - r))
    ]

-- getCell board rowIndex colIndex
--   Returns the contents of the cell at location (rowIndex, colIndex)
--   on the board.
getCell :: Board        -- ^ board
        -> Int          -- ^ rowIndex
        -> Int          -- ^ colIndex
        -> Integer      -- ^ value at rowIndex, colIndex
getCell board row col = board !! (row - 1) !! (col - 1)

-- validIndex board index
--   Returns True if the index (a row or column index) is in range.
validIndex :: Board     -- ^ board
           -> Int       -- ^ index
           -> Bool      -- ^ true if index is in range
validIndex board index = index > 0 && index < length board + 1

-- validLoc board rowIndex colIndex
--   Returns True if location (rowIndex, colIndex) is in range (on the board).
validLoc :: Board       -- ^ board
         -> Int         -- ^ rowIndex
         -> Int         -- ^ colIndex
         -> Bool        -- ^ True if in range of board
validLoc board rowIndex colIndex =
    validIndex board rowIndex && validIndex board colIndex

-- setValue board rowIndex colIndex newValue
--  Returns a board that is the same as the given board except that the new
--  value replaces the number at that row and column.
setValue :: Board       -- ^ board
         -> Int         -- ^ rowIndex
         -> Int         -- ^ colIndex
         -> Integer     -- ^ newValue
         -> Board       -- ^ board with value changed
setValue board rowIndex colIndex newValue =
    let (x, _ : ys) = splitAt (rowIndex - 1) board
    in  let (a, _ : bs) = splitAt (colIndex - 1) (getRow board rowIndex)
        in  x ++ (a ++ newValue : bs) : ys
