import Data.List (delete)
import qualified Data.Set as Set
import Data.Char (isNumber, isSpace)
import Utils
import Grid

type Sensor = Point
type Beacon = Point
type Signal = (Sensor, Int)

parseInput :: String -> [(Sensor, Beacon)]
parseInput =
  let parseCoords [a, b, x, y] = ((a, b), (x, y))
   in map (parseCoords . map read . words . filter (\c -> isNumber c || isSpace c || c=='-')) . lines

signal :: Sensor -> Beacon -> Signal
signal s b = (s, manhattanDistance s b)

isOutsideSignal :: Signal -> Point -> Bool
isOutsideSignal (sensor, distance) p = manhattanDistance sensor p > distance

canContainBeacon :: [Signal] -> Point -> Bool
canContainBeacon signals p = all (`isOutsideSignal` p) signals

outerLayer :: Signal -> Set.Set Point
outerLayer ((x, y), d) = Set.fromList . filter inRange . concatMap (\dist -> [(x + dist, y+d-dist), (x+dist, y-d+dist)]) $ [(-d-1)..(d+1)]

inRange :: Point -> Bool
inRange (x, y) = 0 <= x && x <= 4000000 && 0 <= y && y <= 4000000

neighbours' :: Point -> [Point]
neighbours' (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

possiblePoints :: [Signal] -> [Point]
possiblePoints signals =
  let layers = map outerLayer signals
      intersections = [a `Set.intersection` b | a <- layers, b <- delete a layers]
   in concatMap neighbours' . Set.toList . Set.unions $ intersections
-- To investigate: why do I need to get the neighbours :cry:

part1 :: String -> Int
part1 input =
  let yVal = 2000000
      sensorsAndBeacons = parseInput input
      beacons = map snd sensorsAndBeacons
      signals = map (uncurry signal) sensorsAndBeacons
      points = map (, yVal) [-4044141..4044141*3]
   in length . filter (`notElem` beacons) . filter (not . canContainBeacon signals) $ points

part2 :: String -> Int
part2 input =
  let signals = map (uncurry signal) . parseInput $ input
      possPoints = possiblePoints signals
      (x, y) = head . filter (canContainBeacon signals) $ possPoints
   in 4000000 * x + y

main :: IO ()
main = solve "15" part1 part2
