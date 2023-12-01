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

type Flows = M.Map Valve Int

type Distances = M.Map Valve (M.Map Valve Int)

type OpenValves = M.Map Valve Int

data State = State { openValves :: S.Set Valve, currentValve :: Valve, score :: Int, timeLeft :: Int, bestSoFar :: Int, playersLeft :: Int }

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

startingValve :: Valve
startingValve = "AA"

--maxTime :: Int
--maxTime = 30

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

calcDistances :: [Info] -> Distances
calcDistances infos =
  let relevantValves = (startingValve:) . map valve . filter ((>0) . flow) $ infos
   in M.fromList $ [(a, M.fromList ([(b, calcDistance infos a b) | b <- delete a relevantValves])) | a <- relevantValves]

isValidNext :: Flows -> Distances -> [Valve] -> S.Set Valve -> Valve -> Int -> Valve -> Bool
isValidNext flows distances valves openValves currentValve timeLeft valve
 | valve `S.member` openValves = False
 | timeToMoveAndOpen valve >= timeLeft = False
-- | any (\v -> (timeToMoveAndOpen v < timeToMoveAndOpen valve) && (flowForValve flows v > flowForValve flows valve)) valves = False
-- |
 | otherwise = True
  where
--    maxFlow = maximum $ M.elems flows
    timeToMoveAndOpen v = findDistance distances currentValve v + 1
--    score v = timeToMoveAndOpen v



isValidNextV3 :: Flows -> Distances -> [Valve] -> State -> Valve -> Bool
isValidNextV3 flows distances valves state valve
 | valve `S.member` openValves state = False
 | timeToMoveAndOpen valve >= timeLeft state = False
-- | any (\v -> (timeToMoveAndOpen v < timeToMoveAndOpen valve) && (flowForValve flows v > flowForValve flows valve)) valves = False
-- |
 | otherwise = True
  where
--    maxFlow = maximum $ M.elems flows
    timeToMoveAndOpen v = findDistance distances (currentValve state) v + 1
--    score v = timeToMoveAndOpen v

bestScore :: Flows -> Distances -> [Valve] -> S.Set Valve -> Valve -> Int -> Int
bestScore flows distances valves openValves currentValve timeLeft = bestNextAction
  where
    timeToMoveAndOpen valve = findDistance distances currentValve valve + 1
    getFlow = flowForValve flows
    nextValueScore nextValve =
     if isValidNext flows distances valves openValves currentValve timeLeft nextValve
       then recurseToNextValve nextValve
       else 0
    recurseToNextValve valve =
      let newTimeRemaining = timeLeft - timeToMoveAndOpen valve
          thisScore = newTimeRemaining * flowForValve flows valve
          nextScore = bestScore flows distances valves (S.insert valve openValves) valve newTimeRemaining
       in thisScore + nextScore
    bestNextAction = foldr (max . nextValueScore) 0 valves

bestScore2 :: Flows -> Distances -> [Valve] -> S.Set Valve -> Valve -> Int -> Int -> Int
bestScore2 flows distances valves openValves currentValve timeLeft playersLeft
  | playersLeft == 0 = 0
  | null possibleMoves = bestScore2 flows distances valves openValves startingValve 26 (playersLeft-1)
  | otherwise = bestNextAction
    where
      timeToMoveAndOpen valve = findDistance distances currentValve valve + 1
      recurseToNextValve valve =
        let newTimeRemaining = timeLeft - timeToMoveAndOpen valve
            thisScore = newTimeRemaining * flowForValve flows valve
            nextScore = bestScore2 flows distances valves (S.insert valve openValves) valve newTimeRemaining playersLeft
         in thisScore + nextScore
      possibleMoves = filter (isValidNext flows distances valves openValves currentValve timeLeft) valves
      bestNextAction = foldr (max . recurseToNextValve) 0 possibleMoves

--bestScoreV3 :: Flows -> Distances -> [Valve] -> State -> Int
--bestScoreV3 flows distances valves state
--  | playersLeft state == 0 = 0
--  | null possibleMoves = bestScoreV3 flows distances valves state {currentValve=startingValve, timeLeft=26, playersLeft=playersLeft state - 1}
--  | otherwise = bestNextAction
--    where
--      timeToMoveAndOpen valve = findDistance distances (currentValve state) valve + 1
--      getFlow = flowForValve flows
--      recurseToNextValve valve =
--        let newTimeRemaining = timeLeft state - timeToMoveAndOpen valve
--            thisScore = newTimeRemaining * getFlow valve
--            nextScore = bestScoreV3 flows distances valves state { openValves=S.insert valve (openValves state), currentValve=valve, timeLeft=newTimeRemaining }
--         in thisScore + nextScore
--      possibleMoves = filter (isValidNextV3 flows distances valves state) valves
--      bestNextAction = foldr (max . recurseToNextValve) 0 possibleMoves

findDistance :: Distances -> Valve -> Valve -> Int
findDistance distances v1 v2 = M.findWithDefault 0 v1 . M.findWithDefault M.empty v2 $ distances

flowForValve :: Flows -> Valve -> Int
flowForValve flows v = M.findWithDefault 0 v flows

part1 input =
  let infos = parseInput input
      flows = M.fromList . map (\info -> (valve info, flow info)) $ infos
      distances = calcDistances infos
      valves = M.keys distances
      finished = bestScore2 flows distances valves (S.fromList [startingValve]) startingValve 30 1
   in finished
--   in 1

--part2 :: String -> Int
--part2 input =
--  let infos = parseInput input
--      distances = calcDistances infos
--      flows = M.fromList . filter ((>0) . snd) . map (\info -> (valve info, flow info)) $ infos
--      valves = M.keys distances
--      start = [[]]
--      subsets' = subsets $ map valve infos
--      subsets'' = head $ filter ((==7) . length) subsets'
--      timeForSubset' = bestScore flows distances valves (S.fromList ("AA":subsets'')) "AA" 26
----      timeForSubset = map (\c -> bestScore flows distances valves (S.fromList ("AA":c)) "AA" 26) subsets''
----      finished = until (all (\c -> calcTotalTime distances c >= maxTime)) (concatMap (oneStep distances)) start
----   in reverse . sortOn (calcPressureRelease infos distances) $ finished
--   in timeForSubset'
--   in maximum timeForSubset

part2 input =
  let infos = parseInput input
      flows = M.fromList . map (\info -> (valve info, flow info)) $ infos
      distances = calcDistances infos
      valves = M.keys distances
      finished = bestScore2 flows distances valves (S.fromList [startingValve]) startingValve 26 2
--      finished = bestScoreV3 flows distances valves State {openValves=S.fromList [startingValve], currentValve = startingValve, score=0, timeLeft=30, bestSoFar=0, playersLeft=1}
   in finished

main :: IO ()
main = solve "16" part1 part2
