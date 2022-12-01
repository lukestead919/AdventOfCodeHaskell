import Utils
import Data.List.Split
import Data.List (sort)

parseInput :: String -> [[Int]]
parseInput x = map (map read) (splitWhen (=="") (lines x))

totals :: [[Int]] -> [Int]
totals = map sum

part1 :: String -> Int
part1 = maximum . totals . parseInput

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . totals . parseInput 

main = solve "01" part1 part2