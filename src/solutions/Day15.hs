import Data.Char (isNumber, isSpace)
import Data.List (nub)
import Grid
import Utils

type Sensor = Point

type Beacon = Point

type Signal = (Sensor, Int)

parseInput :: String -> [(Sensor, Beacon)]
parseInput =
  let parseCoords [a, b, x, y] = ((a, b), (x, y))
   in map (parseCoords . map read . words . filter (\c -> isNumber c || isSpace c || c == '-')) . lines

signal :: Sensor -> Beacon -> Signal
signal s b = (s, manhattanDistance s b)

isOutsideSignal :: Signal -> Point -> Bool
isOutsideSignal (sensor, distance) p = manhattanDistance sensor p > distance

canContainBeacon :: [Signal] -> Point -> Bool
canContainBeacon signals p = all (`isOutsideSignal` p) signals

outerLayer :: Signal -> [Point]
outerLayer ((x, y), d) =
  let range = d + 1
   in concatMap
        ( \xDel ->
            let yDel = range - xDel
             in [(x + xDel, y + yDel), (x + xDel, y - yDel)]
        )
        [- range .. range]

inRange :: Point -> Bool
inRange (x, y) = 0 <= x && x <= 4000000 && 0 <= y && y <= 4000000

possiblePoints :: [Signal] -> [Point]
possiblePoints = filter inRange . concatMap outerLayer

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
      (x, y) = head . filter (canContainBeacon signals) . possiblePoints $ signals
   in 4000000 * x + y

main :: IO ()
main = solve "15" part1 part2
