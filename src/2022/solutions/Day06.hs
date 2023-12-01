import Data.List (findIndex, nub)
import Data.Maybe
import Utils

findUniqueSublist :: Int -> String -> Int
findUniqueSublist a x =
  let index = fromJust . findIndex (\sublist -> length (nub sublist) == a) . windows a $ x
   in a + index

part1 :: String -> Int
part1 = findUniqueSublist 4

part2 :: String -> Int
part2 = findUniqueSublist 14

main :: IO ()
main = solve "06" part1 part2
