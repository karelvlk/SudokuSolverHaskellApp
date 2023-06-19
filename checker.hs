module Checker where
import Types
import Utils
import System.IO  
import Control.Monad (forM)
import Data.List (transpose, nub, (\\))

checkerMode :: IO ()
checkerMode = do
    putStrLn "Choose the way to provide Sudoku:"
    putStrLn "1. Load from file"
    putStrLn "2. Write by terminal"
    checkerOption <- getLine
    case read checkerOption of
        1 -> do
            putStrLn "Enter the file path:"
            path <- getLine
            startChecker $ Checker (CheckFromFile path)
        2 -> do
            putStrLn "Enter the Sudoku grid row by row (Use 0 for empty cells):"
            grid <- forM [1..9] $ \_ -> do
                line <- getLine
                return $ map read (words line)
            startChecker $ Checker (CheckByTerminal grid)
        _ -> do
            putStrLn "Invalid choice. Please try again."
            checkerMode

startChecker :: Types.Mode -> IO ()
startChecker (Checker checkerMode) = do
    grid <- case checkerMode of
        CheckFromFile path -> loadGrid path
        CheckByTerminal grid -> return grid
    putStrLn "Loaded the following grid:"
    printGrid grid
    putStrLn "Checking..."
    putStrLn $ if check grid then "This puzzle is valid." else "This puzzle is invalid."


-- Define the Sudoku checking function
check :: Types.Grid -> Bool
check grid = all isUnique rows && all isUnique cols && all isUnique boxes
  where
    rows = grid
    cols = transpose grid
    boxes = [concat [take 3 (drop (j * 3) row) | row <- take 3 (drop (i * 3) grid)] | i <- [0..2], j <- [0..2]]

-- Check if a list contains unique elements (ignoring zeros)
isUnique :: [Int] -> Bool
isUnique xs = let xs' = filter (/= 0) xs in nub xs' == xs'