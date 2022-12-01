import Utils
import Data.List.Split
import Data.List (sort)

--solve :: (Show ans) => Int -> (String -> ans) -> (String -> ans) -> IO ()
--solve q p1 p2 = do
--    let inputFile = "src/Q" ++ show q ++ "/input.txt"
--    handle <- openFile inputFile ReadMode
--    contents <- hGetContents handle
--    print ("Part 1: " ++ show (p1 contents))
--    print ("Part 2: " ++ show (p2 contents))
--    hClose handle

parseInput :: String -> [[Int]]
parseInput x = map (map read) (splitWhen (=="") (lines x))

totals :: [[Int]] -> [Int]
totals = map sum

part1 :: String -> Int
part1 x = maximum $ totals (parseInput x)

part2 :: String -> Int
part2 x = sum $ take 3 (reverse $ sort $ totals (parseInput x))

main = solve "01" part1 part2