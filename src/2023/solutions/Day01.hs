import Data.Char (isDigit)
import Data.List (sort, isPrefixOf, tails)
import Data.List.Split
import Data.List.Utils (replace)
import Utils (solve)
import Debugging

parseInput :: String -> [String]
parseInput = lines

part1 :: String -> Int
part1 = part1' . parseInput
  where
    part1' = sum . map (read . calibrationValue . filter isDigit)
    calibrationValue x = [head x, last x]

part2 :: String -> Int
part2 = part2' . parseInput
  where
    part2' = sum . map (read . calibrationValue . toDigits)
    calibrationValue x = [head x, last x]
    toDigits :: String -> String
    toDigits = filter isDigit . map head . map replaceDigit . init . tails

replaceDigit :: String -> String
replaceDigit x = if (startsWithAny x) then digit x else id x
  where
    startingDigits x = filter (`isPrefixOf` x) digits
    startsWithAny x = length (startingDigits x) > 0
    digit x = (:[]) . asDigit . head $ filter (`isPrefixOf` x) digits

digits :: [String]
digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

asDigit :: String -> Char
asDigit x = case x of
  "one" -> '1'
  "two" -> '2'
  "three" -> '3'
  "four" -> '4'
  "five" -> '5'
  "six" -> '6'
  "seven" -> '7'
  "eight" -> '8'
  "nine" -> '9'

main :: IO ()
main = solve "01" part1 part2
