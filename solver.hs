module Solver where
import Types
import Utils
import System.IO  
import Control.Monad (forM)
import Data.Foldable (asum)
import System.Random (newStdGen, randomRs)

solverMode :: IO ()
solverMode = do
    putStrLn "Choose the way to provide Sudoku:"
    putStrLn "1. Load from file"
    putStrLn "2. Write by terminal"
    solverOption <- getLine
    case read solverOption of
        1 -> do
            putStrLn "Enter the file path:"
            path <- getLine
            startSolver $ Solver (LoadFromFile path)
        2 -> do
            putStrLn "Enter the Sudoku grid row by row (Use 0 for empty cells):"
            grid <- forM [1..9] $ \_ -> do
                line <- getLine
                return $ map read (words line)
            startSolver $ Solver (WriteByTerminal grid)
        _ -> do
            putStrLn "Invalid choice. Please try again."
            solverMode

startSolver :: Types.Mode -> IO ()
startSolver (Solver solverMode) = do
    grid <- case solverMode of
        LoadFromFile path -> loadGrid path
        WriteByTerminal grid -> return grid
    putStrLn "Loaded the following grid:"
    printGrid grid
    putStrLn "Solving..."
    case solve grid of
        [] -> putStrLn "This puzzle can't be solved."
        solutions -> do
            putStrLn $ "Found " ++ show (length solutions) ++ " solution(s):"
            mapM_ printGrid solutions


-- Define the Sudoku solving function, input is semi-filled grid and output is array of possible solutions
solve :: Types.Grid -> [Grid]
solve grid = case emptyCell grid of
    Nothing -> [grid]  -- No empty cell means the grid is solved
    Just (r, c) -> concatMap solve $ fillCell r c grid

solveOneSolution :: Types.Grid -> Maybe Types.Grid
solveOneSolution grid = case emptyCell grid of
    Nothing -> Just grid  -- No empty cell means the grid is solved
    Just (r, c) -> asum [solveOneSolution (replace2D r c n grid) | n <- [1..9], isValid r c n grid]

-- Find an empty cell in the grid
emptyCell :: Types.Grid -> Maybe (Int, Int)
emptyCell grid = case [(i, j) | i <- [0..8], j <- [0..8], grid !! i !! j == 0] of
    [] -> Nothing
    (x:_) -> Just x

-- Fill an empty cell with a valid number
fillCell :: Int -> Int -> Types.Grid -> [Types.Grid]
fillCell r c grid = [replace2D r c n grid | n <- [1..9], isValid r c n grid]

-- Check if a number is valid in a certain cell
isValid :: Int -> Int -> Int -> Types.Grid -> Bool
isValid r c n grid = notElem n (row ++ col ++ box)
  where
    row = grid !! r
    col = map (!! c) grid
    box = concatMap (take 3 . drop (c `div` 3 * 3)) $ take 3 $ drop (r `div` 3 * 3) grid

-- Replace a cell in a 2D list with a new value
replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D r c v xs = replace r newRow xs
  where
    oldRow = xs !! r
    newRow = replace c v oldRow

-- Replace an element in a list with a new value
replace :: Int -> a -> [a] -> [a]
replace i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- Randomly shuffle a list
shuffle :: [a] -> IO [a]
shuffle xs = do
    gen <- newStdGen
    let randomIndices = randomRs (0, length xs - 1) gen
    return $ map (xs !!) randomIndices