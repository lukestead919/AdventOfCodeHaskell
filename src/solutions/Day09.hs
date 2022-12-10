import Data.List (nub)
import Utils

data Direction = L | D | R | U deriving (Show, Read)
type Instruction = (Direction, Int)
type Point = (Int, Int)
type Rope = [Point]

parseInput :: String -> [Instruction]
parseInput = let parseWords x = (read . head $ x, read . last $ x)
                 parseLine = parseWords . words
              in map parseLine . lines

add :: Point -> Point -> Point
add (a, b) (c, d) = (a + c, b + d)

directionAsPoint :: Direction -> Point
directionAsPoint d = case d of
                      R -> (1, 0)
                      L -> (-1, 0)
                      U -> (0, 1)
                      D -> (0, -1)

applyDirection :: Direction -> Rope -> Rope
applyDirection d rope = let newHead = head rope `add` directionAsPoint d
                            newRope = newHead:tail rope
                         in scanl1 moveTailIfNecessary newRope

moveTailIfNecessary :: Point -> Point -> Point
moveTailIfNecessary (hx, hy) (tx, ty) = let dx = hx - tx
                                            dy = hy - ty
                                            move = abs dx > 1 || abs dy > 1
                                            translation = if move then (signum dx, signum dy) else (0, 0)
                                         in (tx, ty) `add` translation

directionList :: [Instruction] -> [Direction]
directionList = concatMap (\(d, num) -> replicate num d)

ropeHistory :: Int -> String -> [Rope]
ropeHistory knots = scanl (flip applyDirection) (replicate knots (0, 0)) . directionList . parseInput

solveForKnots :: Int -> String -> Int
solveForKnots num = length . nub . map last . ropeHistory num

part1 :: String -> Int
part1 = solveForKnots 2

part2 :: String -> Int
part2 = solveForKnots 10

main :: IO ()
main = solve "09" part1 part2
