import Data.List (sort)
import Data.List.Split
import Utils

parseInput :: String -> [[Int]]
parseInput = map (map read) . splitWhen (== "") . lines

totals :: [[Int]] -> [Int]
totals = map sum

part1 :: String -> Int
part1 = maximum . totals . parseInput

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . totals . parseInput

main :: IO ()
main = solve "01" part1 part2
