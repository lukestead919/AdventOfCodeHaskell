import Data.List (sort, findIndex, foldl', delete)
import Data.List.Split
import Utils
import Data.Maybe
import Debugging

parseInput :: String -> [Int]
parseInput = map read . lines

moveNum :: Int -> [(Int, Int)] -> [(Int, Int)]
moveNum a x = new
  where
    currentIndex = fromJust $ findIndex ((==a) . fst) x
    value@(_, val) = x !! currentIndex
    newIndex = (val + currentIndex) `mod` (length x - 1)
    removed = delete value x
    (s, e) = splitAt newIndex removed
    new = s ++ value : e

part1 input = answer
  where
    list = parseInput input
    indexes = [0..length list - 1]
    listWithIndexes = zip indexes list
    mixed = foldl' (flip moveNum) listWithIndexes indexes
    indexOf0 = fromJust $ findIndex ((==0) . snd) mixed
    answer = sumBy (\i -> snd $ mixed !! ((indexOf0 + i) `mod` length list)) [1000, 2000, 3000]

part2 input = answer
  where
    list = map (*811589153) $ parseInput input
    indexes = [0..length list - 1]
    listWithIndexes = zip indexes list
    mix = flip (foldl' (flip moveNum)) indexes
    mixed = iterate mix listWithIndexes !! 10
    indexOf0 = fromJust $ findIndex ((==0) . snd) mixed
    answer = sumBy (\i -> snd $ mixed !! ((indexOf0 + i) `mod` length list)) [1000, 2000, 3000]

main :: IO ()
main = solve "20" part1 part2
