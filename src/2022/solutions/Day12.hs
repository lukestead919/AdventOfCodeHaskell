import qualified Data.Map as Map
import Data.Maybe
import Grid
import Utils

data PointState = PointState {point :: Point, visited :: Bool, distance :: Int} deriving (Eq, Show)

instance Ord PointState where
  p1 <= p2 = distance p1 <= distance p2

type State = Map.Map Point PointState

parseInput :: String -> [[Char]]
parseInput = lines

step :: Grid Char -> State -> State
step grid state =
  let minPointState = minimum . filter (not . visited) . Map.elems $ state
      minPoint = point minPointState
      minPointValue = gridValue grid minPoint
      validNeighbour nbr = minPointValue <= succ (gridValue grid nbr)
      neighbours' = filter validNeighbour . neighbours grid $ minPoint
      neighbourState = map (\nbr -> (nbr, PointState {point = nbr, visited = False, distance = distance minPointState + 1})) neighbours'
   in Map.union (Map.insert minPoint (minPointState {visited=True}) state) (Map.fromList neighbourState)

iterateToEnd :: Grid Char -> Char -> State
iterateToEnd grid start =
  let end = fromJust (findValue grid 'E')
      initialState = Map.fromList [(end, PointState {point = end, visited = False, distance = 0})]
      fixedGrid = replaceInGrid 'S' 'a' . replaceInGrid 'E' 'z' $ grid
      containsStart = any (\c -> gridValue grid c == start)
      finishedState = until (containsStart . Map.keys) (step fixedGrid) initialState
   in finishedState

part1 :: String -> Int
part1 = distance . maximum . flip iterateToEnd 'S' . parseInput

part2 :: String -> Int
part2 = distance . maximum . flip iterateToEnd 'a' . parseInput

main :: IO ()
main = solve "12" part1 part2
