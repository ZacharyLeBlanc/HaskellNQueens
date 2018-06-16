{- |
Module      : Main
Description : This module is for running the NQueens program.
Author      : Zach LeBlanc

This module is to solve the N Queens problem in Haskell. It prompts the user
for input for N in the N queens problem then prints out the board with the
solution. 

To run this you will first have to load the module into GHCI with
`:l Main` then run the `main` function in the prompt. However you may need to
move all of the files into the same folder.

Recommended way:
If you are using stack then you can run `stack build` then type in `stack exec
Nqueens-exe` in the terminal.

You can also use GHC however you may run into the same problems as using GHCI.
-}

module Main
where

import           NQueens

-- | main
-- | This function prompts the user for a number input for the N queens problem
-- | then solves the problem for the given input then prints out the solution
-- | as a board. This will throw a parse error if not given a valid Int.
main :: IO ()
main = do
    putStrLn "Enter number for N Queens:"
    s <- getLine
    let i = read s :: Int
    print (solveNQueens i)
