import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split
import Utils

parseInput :: String -> [String]
parseInput = lines

intersectStrings :: (String, String) -> String
intersectStrings = uncurry intersect

half :: String -> (String, String)
half x =
  let midway = length x `div` 2
   in splitAt midway x

charValue :: Char -> Int
charValue x =
  let ascii = ord x
   in if ascii >= 97
        then ascii - 96
        else ascii - 38

part1 :: String -> Int
part1 =
  let score = charValue . head . intersectStrings . half
   in sum . map score . parseInput

part2 :: String -> Int
part2 =
  let score = charValue . head . foldr1 intersect
   in sum . map score . chunksOf 3 . parseInput

main :: IO ()
main = solve "03" part1 part2
