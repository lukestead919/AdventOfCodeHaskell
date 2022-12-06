import Data.List (findIndex, nub)
import Data.Maybe
import Utils

findUniqueList :: Int -> String -> Int
findUniqueList a x =
  let sublists = [take a . drop i $ x | i <- [0 .. (length x)]]
      index = fromJust . findIndex (\sublist -> length (nub sublist) == a) $ sublists
   in a + index

part1 :: String -> Int
part1 = findUniqueList 4

part2 :: String -> Int
part2 = findUniqueList 14

main :: IO ()
main = solve "06" part1 part2
