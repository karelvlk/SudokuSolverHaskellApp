module Utils where 
import System.IO 
import System.Directory (doesFileExist)

loadGrid :: FilePath -> IO [[Int]]
loadGrid path = do
    handle <- openFile path ReadMode
    content <- hGetContents handle
    return $ map (map read . words) (lines content)

printGrid :: Show a => [[a]] -> IO ()
printGrid = mapM_ (putStrLn . unwords . map show)
