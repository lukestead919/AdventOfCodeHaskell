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

type Distance = M.Map Valve (M.Map Valve Int)

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

currentValve' :: [Action] -> Valve
currentValve' [] = startingValve
currentValve' (x:xs) = case x of
                        Move _ to -> to
                        _ -> currentValve' xs

findCurrentValve :: [Action] -> Valve
findCurrentValve = currentValve' . reverse

isValveOpen :: [Action] -> Valve -> Bool
isValveOpen a v = isJust . find (==Open v) $ a

thingToList :: a -> [a]
thingToList a = [a]

toInv :: [Action]
toInv = read "[Move (\"AA\",\"DD\"),Open \"DD\",Move (\"DD\",\"BB\"),Open \"BB\",Move (\"BB\",\"JJ\"),Open \"JJ\",Move (\"JJ\",\"HH\"),Open \"HH\",Move (\"HH\",\"EE\"),Open \"EE\",Move (\"EE\",\"CC\"),Open \"CC\"]" :: [Action]

bestPath :: [Info] -> Distance -> [Action] -> [Action]
bestPath infos distances actions
  | currentTime >= maxTime = actions
  | null nextActions = actions ++ [Wait]
  | otherwise = bestNextAction
  where
    recursiveStep = bestPath infos distances
    currentValve = findCurrentValve actions
    currentTime = calcTotalTime distances actions
    timeToMoveAndOpen valve = findDistance distances currentValve valve + 1
    filterAvailableValves = filter (\c -> currentTime + timeToMoveAndOpen c < maxTime) . filter (not . isValveOpen actions)
    validMoves = filterAvailableValves . M.keys . M.findWithDefault M.empty currentValve $ distances
    nextActions = map (\move -> actions ++ [Move currentValve move, Open move]) validMoves
    bestNextAction = last . sortOn (calcPressureRelease infos distances) . map recursiveStep $ nextActions
    
--bestPath :: [Info] -> Distance -> S.Set Valve -> Int -> Int
--bestPath infos distances openValves timeLeft
--  | timeLeft <= 0 = 0
--  | not (null actions) && (not . isValveOpen actions $ currentValve) = recursiveStep (actions ++ [Open currentValve])
--  | null nextActions = actions ++ [Wait]
--  | otherwise = bestNextAction
--  where
--    recursiveStep = bestPath infos distances
--    currentValve = findCurrentValve actions
--    currentTime = calcTotalTime distances actions
--    validMoves = filter (not . isValveOpen actions) . M.keys . M.findWithDefault M.empty currentValve $ distances
--    nextActions = map ((actions++) . thingToList . Move currentValve) validMoves
--    bestNextAction = last . sortOn (calcPressureRelease infos distances) . map recursiveStep $ nextActions

--oneStep :: Distance -> [Action] -> [[Action]]
--oneStep distances actions =
--  let currentValve = findCurrentValve actions
--      validMoves = filter (not . isValveOpen actions) . M.keys . M.findWithDefault M.empty currentValve $ distances
--      currentTime = calcTotalTime distances actions
--      nextActions = map ((actions++) . thingToList . Move currentValve) validMoves
--   in if currentTime >= maxTime then [actions] else
--         if not (null actions) && (not . isValveOpen actions $ currentValve) then [actions ++ [Open currentValve]]
--         else nextActions

findDistance :: Distance -> Valve -> Valve -> Int
findDistance distances v1 v2 = M.findWithDefault 0 v1 . M.findWithDefault M.empty v2 $ distances

actionTime :: Distance -> Action -> Int
actionTime distances x = case x of
                          Open _ -> 1
                          Move from to -> findDistance distances from to
                          Wait -> maxTime

calcTimes :: Distance -> [Action] -> [Int]
calcTimes distances a = let timePerAction = map (actionTime distances) a in tail $ scanl (+) 0 timePerAction

calcTotalTime :: Distance -> [Action] -> Int
calcTotalTime d a = let times = calcTimes d a in if null times then 0 else last times

flowForAction :: [Info] -> Action -> Int
flowForAction infos (Open v) = flow . fromJust . find (\c -> valve c == v) $ infos
flowForAction _ _ = 0
  
calcPressureRelease' :: [Info] -> Distance -> Int -> Int -> [Action] -> Int
calcPressureRelease' _ _ _ total [] = total
calcPressureRelease' info distance timeLeft total (x:xs)
  | timeLeft <= 0 = total
  | otherwise =
  let timeTaken = actionTime distance x
      newTimeLeft = timeLeft - timeTaken
   in calcPressureRelease' info distance newTimeLeft ((flowForAction info x * newTimeLeft) + total) xs

calcPressureRelease :: [Info] -> Distance -> [Action] -> Int
calcPressureRelease infos distances a = calcPressureRelease' infos distances maxTime 0 a

--part1 :: String -> Int
--part1 input =
--  let infos = parseInput input
--      distances = calcDistances infos
--      start = [[]]
--      finished = until (all (\c -> calcTotalTime distances c >= maxTime)) (concatMap (oneStep distances)) start
--   in maximum . map (calcPressureRelease infos distances) $ finished

--part1 :: String -> Int
part1 input =
  let infos = parseInput input
      distances = calcDistances infos
      start = []
      finished = bestPath infos distances start
   in calcPressureRelease infos distances finished
--   in 1
--   in distances
--   in (calcPressureRelease infos distances finished, finished, map (flowForAction infos) finished, calcTimes distances finished)


--part2 :: String -> Int
part2 input =
  let infos = parseInput input
      distances = calcDistances infos
      start = [[]]
--      finished = until (all (\c -> calcTotalTime distances c >= maxTime)) (concatMap (oneStep distances)) start
--   in reverse . sortOn (calcPressureRelease infos distances) $ finished
   in 1

main :: IO ()
main = solve "16" part1 part2
