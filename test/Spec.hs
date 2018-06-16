module Main
    ( main
    )
where

import           TestBoard
import           TestNQueens

main :: IO ()
main = do
    runTestsBoard
    runTestsNQueens
