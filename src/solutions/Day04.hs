import Data.List.Split
import Utils

data Range = Range Int Int

parseInput :: String -> [(Range, Range)]
parseInput =
  let parseRange = uncurry Range . toPair . map (read :: String -> Int) . splitOn "-"
      parseLine = toPair . map parseRange . splitOn ","
   in map parseLine . lines

overlapFully :: Range -> Range -> Bool
overlapFully (Range a b) (Range c d) = (a <= c && b >= d) || (a >= c && b <= d)

overlap :: Range -> Range -> Bool
overlap (Range a b) (Range c d) = a <= d && b >= c

part1 :: String -> Int
part1 = length . filter (uncurry overlapFully) . parseInput

part2 :: String -> Int
part2 = length . filter (uncurry overlap) . parseInput

main :: IO ()
main = solve "04" part1 part2
