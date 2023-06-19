module Generator where
import Types
import Utils
import Solver
import System.IO  
import System.Random.Shuffle (shuffle')
import System.Random (newStdGen)

generatorMode :: IO ()
generatorMode = do
    putStrLn "Choose difficulty level:"
    putStrLn "1. Easy"
    putStrLn "2. Medium"
    putStrLn "3. Hard"
    putStrLn "4. Custom"
    difficulty <- getLine
    case reads difficulty :: [(Int, String)] of
        [(n, _)] -> case n of
            1 -> startGenerator $ Generator Easy
            2 -> startGenerator $ Generator Medium
            3 -> startGenerator $ Generator Hard
            4 -> do
                putStrLn "Enter the number of cells between 1 and 64 to remove (there must be at least 17 filled cells to have unique solution):"
                customDifficulty <- getLine
                case reads customDifficulty :: [(Int, String)] of
                    [(n, _)] -> if n >= 1 && n <= 64
                        then startGenerator $ Generator (Custom n)
                        else invalidChoice
                    _ -> invalidChoice
            _ -> invalidChoice
        _ -> invalidChoice
  where
    invalidChoice = do
        putStrLn "Invalid choice. Please try again."
        generatorMode

startGenerator :: Types.Mode -> IO ()
startGenerator (Generator difficulty) = do
    putStrLn $ "Starting Generator in mode: " ++ show difficulty
    generatedGrid <- generate (difficultyValue difficulty)
    printGrid generatedGrid
  where
    difficultyValue Easy = 20
    difficultyValue Medium = 40
    difficultyValue Hard = 60
    difficultyValue (Custom n) = n

generateFullGrid :: IO Types.Grid
generateFullGrid = do
    g <- newStdGen
    let shuffledNumbers = shuffle' [1..9] 9 g
    let startGrid = shuffledNumbers : replicate 8 (replicate 9 0)
    case solveOneSolution startGrid of
        Just grid -> return grid
        Nothing -> error "Failed to generate a full grid"

-- Define the Sudoku generating function
generate :: Int -> IO Types.Grid
generate difficulty = do
    fullGrid <- generateFullGrid
    let positions = [(x, y) | x <- [0..8], y <- [0..8]]
    g <- newStdGen
    let shuffledPositions = shuffle' positions (length positions) g
    removeCells fullGrid shuffledPositions difficulty

removeCells :: Types.Grid -> [(Int, Int)] -> Int -> IO Types.Grid
removeCells grid [] _ = return grid
removeCells grid ((x, y):xs) difficulty
    | difficulty <= 0 = return grid
    | otherwise = do
        let newGrid = removeCell grid x y
        let solutions = solve newGrid
        if length solutions == 1
            then removeCells newGrid xs (difficulty - 1)
            else removeCells grid xs difficulty

removeCell :: Types.Grid -> Int -> Int -> Types.Grid
removeCell grid x y = take x grid ++ [take y (grid !! x) ++ [0] ++ drop (y + 1) (grid !! x)] ++ drop (x + 1) grid