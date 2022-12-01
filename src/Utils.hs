module Utils
    ( solve
    ) where

import System.IO

solve :: (Show ans) => String -> (String -> ans) -> (String -> ans) -> IO ()
solve q p1 p2 = do
    let inputFile = "src/day" ++ q ++ "/input.txt"
    handle <- openFile inputFile ReadMode
    contents <- hGetContents handle
    putStrLn ("Part 1: " ++ show (p1 contents))
    putStrLn ("Part 2: " ++ show (p2 contents))
    hClose handle
