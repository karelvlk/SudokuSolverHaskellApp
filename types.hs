module Types where
import System.IO  

type Grid = [[Int]]
type Cell = (Int, Int)

data Mode = Generator Difficulty | Solver SolverMode | Checker CheckerMode deriving (Show)

data SolverMode = LoadFromFile FilePath | WriteByTerminal Grid deriving (Show)

data Difficulty = Easy | Medium | Hard | Custom Int deriving (Show)

data CheckerMode = CheckFromFile FilePath | CheckByTerminal Grid deriving (Show)

