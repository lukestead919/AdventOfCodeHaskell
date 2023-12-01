import Data.List (transpose)
import Utils

type Grid = [String]

type GridSize = (Int, Int)

type Point = (Int, Int)

parseInput :: String -> Grid
parseInput = lines

gridSize :: Grid -> GridSize
gridSize x = (length x, length (head x))

blockingTrees :: Point -> Grid -> [String]
blockingTrees (x, y) g =
  let (left, right) = splitAt x (g !! y)
      (up, down) = splitAt y (transpose g !! x)
   in [reverse left, tail right, reverse up, tail down]

isVisible :: Point -> Grid -> Bool
isVisible (x, y) g =
  let value = g !! y !! x
      trees = blockingTrees (x, y) g
   in any (all (< value)) trees

visScore :: Point -> Grid -> Int
visScore (x, y) g =
  let value = g !! y !! x
      trees = blockingTrees (x, y) g
      score = length . takeUntil (< value)
   in product . map score $ trees

part1 :: String -> Int
part1 input =
  let grid = parseInput input
      (a, b) = gridSize grid
      vis = [[isVisible (x, y) grid | y <- [0 .. b -1]] | x <- [0 .. a -1]]
   in sumBy (length . filter id) vis

part2 :: String -> Int
part2 input =
  let grid = parseInput input
      (a, b) = gridSize grid
      visScores = [[visScore (x, y) grid | y <- [0 .. b -1]] | x <- [0 .. a -1]]
   in maximum . map maximum $ visScores

main :: IO ()
main = solve "08" part1 part2
