import Data.List (sort, isPrefixOf)
import Data.List.Split
import Utils

parseInput :: String -> [String]
parseInput = lines

parseInstruction :: String -> [Int] -> [Int]
parseInstruction instr vals
 | "addx " `isPrefixOf` instr = [val, val + (read . last . words $ instr)]
 | instr == "noop" = [val]
 where val = last vals

parse :: String -> [Int]
parse = concat . scanl (flip parseInstruction) [1] . parseInput

part1 :: String -> Int
part1 input = let score ls x = x * ls !! (x-1)
                  values = parse input
               in sumBy (score values . subtract 20 . (*40)) [1..6]

display :: [Int] -> [String]
display x = let isDisplayed ls cycle = abs ((ls !! cycle) - (cycle `mod` 40)) <= 1
             in chunksOf 40 . map (\c -> if isDisplayed x c then '#' else '.') $ [0..239]

part2 :: String -> [String]
part2 = display . parse

main :: IO ()
main = solve "10" part1 part2

-- ####.###..#..#.###..#..#.####..##..#..#.
-- #....#..#.#..#.#..#.#..#....#.#..#.#..#.
-- ###..###..#..#.#..#.####...#..#....####.
-- #....#..#.#..#.###..#..#..#...#....#..#.
-- #....#..#.#..#.#.#..#..#.#....#..#.#..#.
-- #....###...##..#..#.#..#.####..##..#..#.