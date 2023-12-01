import Data.List (nub)
import Data.List.Split
import Data.Set (Set, elems, empty, fromList, insert, member)
import Grid
import Utils

type Path = [Point]

type Rocks = Set Point

type Sand = Set Point

sandStartingPoint :: Point
sandStartingPoint = (500, 0)

parseInput :: String -> [Path]
parseInput = map (map (toPair . map read . splitOn ",") . splitOn " -> ") . lines

getRocks :: [Path] -> Rocks
getRocks paths =
  let getRocksForPath = nub . concatMap (uncurry getPointsBetween) . zipWithNext
   in fromList . nub . concatMap getRocksForPath $ paths

sandFall :: Rocks -> Sand -> Point -> Point
sandFall rocks sand (x, y) =
  let isOccupied point = point `member` rocks || point `member` sand
   in head . filter (not . isOccupied) $ [(x, y + 1), (x -1, y + 1), (x + 1, y + 1), (x, y)]

sandRestingPoint :: Rocks -> Sand -> Either String Point
sandRestingPoint rocks sand =
  let lowestRock = maximum . map snd . elems $ rocks
      sandFall' = sandFall rocks sand
      restingPoint = until (\c -> sandFall' c == c || snd c > lowestRock) sandFall' sandStartingPoint
   in if sandStartingPoint `member` sand
        then Left "sand full"
        else
          if snd restingPoint > lowestRock
            then Left "falling forever"
            else Right restingPoint

dropOneSand :: Rocks -> Sand -> Sand
dropOneSand rocks sand =
  let restingPoint = sandRestingPoint rocks sand
   in case restingPoint of
        Left _ -> sand
        Right x -> insert x sand

dropSandUntilDone :: Rocks -> Sand
dropSandUntilDone rocks =
  let dropSand' = dropOneSand rocks
   in until (\c -> dropSand' c == c) dropSand' empty

part1 :: String -> Int
part1 = length . dropSandUntilDone . getRocks . parseInput

part2 :: String -> Int
part2 input =
  let inputRocks = getRocks . parseInput $ input
      yFloor = (+ 2) . maximum . map snd . elems $ inputRocks
      xMinFloor = 500 - yFloor - 1
      xMaxFloor = 500 + yFloor + 1
      rocks = foldl (flip insert) inputRocks (getPointsBetween (xMinFloor, yFloor) (xMaxFloor, yFloor))
   in length . dropSandUntilDone $ rocks

main :: IO ()
main = solve "14" part1 part2
