{-# LANGUAGE LambdaCase #-}

import Data.List (sort, (\\))
import qualified Data.Map as M
import Data.Char (isNumber, isSpace)
import Data.List.Split
import Debugging
import Utils
import Data.Maybe

data Material = Geode  | Obsidian | Clay | Ore  deriving (Ord, Eq, Show)

type Stockpile = M.Map Material Int

type Robots = M.Map Material Int

type Recipe = M.Map Material Int

type Blueprint = M.Map Material Recipe

data State = State { stockpile :: Stockpile, robots :: Robots, lastTurnRobot :: Maybe Material, time :: Int } deriving (Show)

maxTime :: Int
maxTime = 32

exampleBlueprint1 :: Blueprint
exampleBlueprint1 = M.fromList [(Ore, M.fromList [(Ore, 4)]), (Clay, M.fromList [(Ore, 2)]), (Obsidian, M.fromList [(Ore, 3), (Clay, 14)]), (Geode, M.fromList [(Ore, 2), (Obsidian, 7)])]

exampleBlueprint2 :: Blueprint
exampleBlueprint2 = M.fromList [(Ore, M.fromList [(Ore, 2)]), (Clay, M.fromList [(Ore, 3)]), (Obsidian, M.fromList [(Ore, 3), (Clay, 8)]), (Geode, M.fromList [(Ore, 3), (Obsidian, 12)])]

parseInput :: String -> [Blueprint]
parseInput = map parseBlueprint . lines
  where
    parseBlueprint = (\w -> M.fromList [(Ore, M.fromList [(Ore, intAt w 6)]), (Clay, M.fromList [(Ore, intAt w 12)]), (Obsidian, M.fromList [(Ore, intAt w 18), (Clay, intAt w 21)]), (Geode, M.fromList [(Ore, intAt w 27), (Obsidian, intAt w 30)])]) . words
    intAt w i = read (w !! i) :: Int

stored :: Material -> Stockpile -> Int
stored = M.findWithDefault 0

canAfford :: Recipe -> Stockpile -> Bool
canAfford = M.isSubmapOfBy (<=)

spendMaterials :: Recipe -> Stockpile -> Stockpile
spendMaterials = flip $ M.differenceWith (\a  b -> Just (a-b))

gatherMaterials :: Robots -> Stockpile -> Stockpile
gatherMaterials = M.unionWith (+)

affordableRobots :: Blueprint -> Stockpile -> [Material]
affordableRobots blueprint stockpile = M.keys . M.filter (`canAfford` stockpile) $ blueprint

addRobot :: Material -> Robots -> Robots
addRobot material = M.insertWith (+) material 1

maxRobots :: Material -> Int
maxRobots material = case material of
                       Ore -> 4
                       Clay -> 10
                       Obsidian -> 10
                       Geode -> 99
                       
cutoff :: Material -> Int
cutoff material = case material of 
                    Ore -> 10
                    Clay -> 20
                    Obsidian -> maxTime
                    Geode -> maxTime

cullOptions :: Blueprint -> State -> [Maybe Material] -> [Maybe Material]
cullOptions blueprint State {stockpile, robots, lastTurnRobot, time} options
  | Just Geode `elem` options = [Just Geode]
  | time >= maxTime - 2 = [Nothing] -- Geode only at this point (captured above)
  | otherwise = culledOptions
  where
--    safe filters
    oreRobotCost = M.findWithDefault 0 Ore . M.findWithDefault M.empty Ore $ blueprint
    oreRoiCutoff = if time + oreRobotCost + 2 >= maxTime then filter (/=Just Ore) else id -- +1 for building the robot, +1 for net positive ROI

    clayCutoff = if time >= maxTime - 3 then filter (/=Just Clay) else id
    

    lastTurnStockpile = spendMaterials robots stockpile
    lastTurnAvailable = affordableRobots blueprint lastTurnStockpile
    lastTurnPassedOn = if isNothing lastTurnRobot then map Just lastTurnAvailable else []
    filterPassedOnLastTurn = (\\ lastTurnPassedOn)

    turnsLeft = maxTime - time
    clayCost = (M.findWithDefault 0 Clay . M.findWithDefault M.empty Obsidian $ blueprint) - M.findWithDefault 0 Clay robots
    turnsOfClayLeft = stored Clay stockpile `div` clayCost
    clayFull = if turnsLeft < 10 && turnsOfClayLeft >= turnsLeft then filter (/=Just Clay) else id

    safeFilters = oreRoiCutoff . filterPassedOnLastTurn . clayCutoff

--    unsafe filters
    limitMaxRobots = filter (\case {Nothing -> True; Just m -> M.findWithDefault 0 m robots <= maxRobots m})
    cutoffs = filter (\case {Nothing -> True; Just m -> time <= cutoff m})
    cullSlowObsidian x = if time > 20 && stored Obsidian stockpile == 0 then [] else x
    preferObsidian x = if Just Obsidian `elem` x then [Nothing, Just Obsidian] else x
    
    unsafeFilters = limitMaxRobots . cullSlowObsidian . cutoffs

--    culledOptions = safeFilters options
    culledOptions = unsafeFilters . safeFilters $ options

bestGeodes :: Blueprint -> State -> Int
bestGeodes blueprint currentState@State {stockpile, robots, time}
  | time == maxTime = currentGeodes
  | null viableOptions = currentGeodes
  | otherwise = bestNextIteration
  where
    currentGeodes = stored Geode stockpile
    possibleOptions = map Just (affordableRobots blueprint stockpile) ++ [Nothing]
    viableOptions = cullOptions blueprint currentState possibleOptions
    nextTime = time + 1
    nextIteration purchasedRobot = case purchasedRobot of
      Nothing -> bestGeodes blueprint currentState {stockpile=gatherMaterials robots stockpile, time=nextTime, lastTurnRobot=Nothing}
      Just robot -> bestGeodes blueprint currentState {stockpile=spendMaterials (fromJust $ M.lookup robot blueprint) . gatherMaterials robots $ stockpile
                                                     , robots=addRobot robot robots
                                                     , time=nextTime
                                                     , lastTurnRobot=Just robot}
    bestNextIteration = maximum $ map nextIteration viableOptions
--    bestNextIteration = traceLns possibleOptions head $ map nextIteration possibleOptions

startingState :: State
startingState = State {stockpile=M.empty, robots=M.singleton Ore 1, time=0, lastTurnRobot=Nothing}

--part1 :: String -> Int
--part1 input = bestGeodes exampleBlueprint1 State {stockpile=M.empty, robots=M.singleton Ore 1, time=0, lastTurnRobot=Nothing}
--part1 input = bestGeodes exampleBlueprint2 State {stockpile=M.empty, robots=M.singleton Ore 1, time=0, lastTurnRobot=Nothing}
part1 input = 1
  where
    blueprints = parseInput input
    geodes = map (`bestGeodes` startingState) blueprints
    scores = zipWith (*) [1..] geodes
    answer = sum scores

--part2 :: String -> Int
part2 input = answer
  where
    blueprints = take 3 $ parseInput input
    geodes = map (`bestGeodes` startingState) blueprints
--    answer = product geodes
    answer = geodes

main :: IO ()
main = solve "19" part1 part2
