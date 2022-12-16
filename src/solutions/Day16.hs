import Data.List (sort, delete, find, sortOn)
import Data.List.Split
import Dijkstra
import Utils
import qualified Data.Map as M
import Data.Maybe
import Debugging
import qualified Data.Set as S

type Valve = String

data Action = Move Valve Valve | Open Valve | Wait deriving (Eq, Show, Read)

data Info = Info { valve :: Valve, flow :: Int, tunnels :: [Valve]}

type Infos = M.Map Valve Info

type Distance = M.Map Valve (M.Map Valve Int)

type OpenValves = M.Map Valve Int

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

startingValve :: Valve
startingValve = "AA"

maxTime :: Int
maxTime = 30

parseInput :: String -> [Info]
parseInput =
  let parseValve = (!! 1)
      parseFlowRate = read . drop 5 . delete ';' . (!! 4)
      parseTunnels = map (delete ',') . drop 9
      parseInfo x = Info {valve=parseValve x, flow=parseFlowRate x, tunnels=parseTunnels x}
   in map (parseInfo . words) . lines

calcDistance :: [Info] -> Valve -> Valve -> Int
calcDistance infos start end =
  let getNeighbours v = M.fromList . map (, 1) . tunnels . fromJust . find (\c -> valve c == v) $ infos
      distance = fromIntegral . fromJust . dijkstra start getNeighbours $ (==end)
   in distance

calcDistances :: [Info] -> Distance
calcDistances infos =
  let relevantValves = (startingValve:) . map valve . filter ((>0) . flow) $ infos
   in M.fromList $ [(a, M.fromList ([(b, calcDistance infos a b) | b <- delete a relevantValves])) | a <- relevantValves]

bestPath :: Infos -> Distance -> S.Set Valve -> Valve -> Int -> Int
bestPath infos distances openValves currentValve timeLeft
  | null nextActions = 0
  | otherwise = bestNextAction
  where
    timeToMoveAndOpen valve = findDistance distances currentValve valve + 1
--    getFlow = flowForValve infos
    recurseToNextValve valve =
      let newTimeRemaining = timeLeft - timeToMoveAndOpen valve
       in (newTimeRemaining * flowForValve infos valve) + bestPath infos distances (S.insert valve openValves) valve newTimeRemaining
    filterAvailableValves = filter (\c -> timeLeft > timeToMoveAndOpen c) . filter (`S.notMember` openValves)
    validMoves = filterAvailableValves . M.keys . M.findWithDefault M.empty currentValve $ distances
--    filterWrongValves valve = any (\v -> getFlow v > getFlow valve && timeToMoveAndOpen v < timeToMoveAndOpen valve) validMoves
    nextActions = map recurseToNextValve validMoves
    bestNextAction = maximum nextActions

findDistance :: Distance -> Valve -> Valve -> Int
findDistance distances v1 v2 = M.findWithDefault 0 v1 . M.findWithDefault M.empty v2 $ distances

flowForValve :: Infos -> Valve -> Int
flowForValve infos v = flow . fromJust . M.lookup v $ infos

part1 input =
  let infos = parseInput input
      infos' = M.fromList . map (\info -> (valve info, info)) $ infos
      distances = calcDistances infos
      finished = bestPath infos' distances (S.fromList ["AA"]) "AA" maxTime
   in finished

--part2 :: String -> Int
part2 input =
  let infos = parseInput input
      distances = calcDistances infos
      infos' = M.fromList . map (\info -> (valve info, info)) $ infos
      start = [[]]
      subsets' = subsets $ map valve infos
      timeForSubset = map (\c -> bestPath infos' distances (S.fromList ("AA":c)) "AA" maxTime) subsets'
--      finished = until (all (\c -> calcTotalTime distances c >= maxTime)) (concatMap (oneStep distances)) start
--   in reverse . sortOn (calcPressureRelease infos distances) $ finished
   in 1
--   in maximum timeForSubset

main :: IO ()
main = solve "16" part1 part2
