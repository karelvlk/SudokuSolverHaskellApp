module Main where
import Solver
import Generator
import Checker
import System.IO  

main :: IO ()
main = do
    putStrLn "Welcome to Sudoku Solver Haskell App!"
    putStrLn "Please choose the mode:"
    putStrLn "1. Generator Mode"
    putStrLn "2. Solver Mode"
    putStrLn "3. Checker Mode"
    choice <- getLine
    case reads choice :: [(Int, String)] of
        [(n, _)] -> case n of
            1 -> generatorMode
            2 -> solverMode
            3 -> checkerMode
            _ -> invalidChoice
        _ -> invalidChoice
  where
    invalidChoice = do
        putStrLn "Invalid choice. Please try again."
        main