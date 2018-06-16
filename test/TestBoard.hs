module TestBoard
    (runTestsBoard
    )
where

import Board
import TestSuiteSupportModule

runTestsBoard :: IO ()
runTestsBoard = do
    putStrLn ""
    putStrLn "Board Testing:"
    print testMkRow
    putStrLn ""
    print testMkBoard
    putStrLn ""
    print testMkTestBoard
    putStrLn ""
    print testGetRow
    putStrLn ""
    print testGetCol
    putStrLn ""

testMkRow = TestSuite
    "Test mkRow"
    [Test "Empty list" (mkRow 0 == []), Test "mkRow 3" (mkRow 3 == [0, 0, 0])]

testMkBoard = TestSuite
    "Test mkBoard"
    [
        Test "empty board" (mkBoard 0 == []),
        Test "mkBoard 3" (mkBoard 3 == [[0,0,0],[0,0,0],[0,0,0]])
    ]

testMkTestBoard = TestSuite
    "Test mkTestBoard"
    [
        Test "empty board" (mkTestBoard 0 == []),
        Test "mkBoard 3" (mkTestBoard 3 == [[11,12,13],[21,22,23],[31,32,33]])
    ]

testGetRow = TestSuite
    "Test getRow"
    [
        -- Throws index too large error which is correct. Will throw error when index is out of range.
        -- Test "getRow on empty board" ((getRow (mkTestBoard 0) 1) == []),
        Test "getRow on first row" (getRow (mkTestBoard 3) 1 == [11,12,13])
    ]

testGetCol = TestSuite
    "Test getCol"
    [
        -- Throws index too large error which is correct. Will throw error when index is out of range.
        -- Test "getCol on empty board" ((getCol (mkTestBoard 0) 1) == []),
        Test "getCol on first row" (getCol (mkTestBoard 3) 1 == [11,21,31])
    ]