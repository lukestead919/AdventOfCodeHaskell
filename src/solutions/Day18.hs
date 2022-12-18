import Data.List (sort, tails)
import Data.List.Split
import Utils

type Point3D = (Int, Int, Int)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

parseInput :: String -> [Point3D]
parseInput =
  let parseLine x = read ("(" ++ x ++ ")")
   in map parseLine . lines

touching :: Point3D -> Point3D -> Bool
touching (x1, y1, z1) (x2, y2, z2) =
  let diffx = abs (x1-x2)
      diffy = abs (y1-y2)
      diffz = abs (z1-z2)
   in diffx + diffy + diffz == 1

part1 :: String -> Int
part1 input =
  let points = parseInput input
   in (6 * length points) - (2 * (length . filter (uncurry touching) . pairs $ points))


part2 :: String -> Int
part2 x = 1

main :: IO ()
main = solve "18" part1 part2
