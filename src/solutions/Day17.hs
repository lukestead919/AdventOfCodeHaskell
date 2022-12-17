import Data.List (sort, foldl')
import Data.List.Split
import Utils
import qualified Data.Set as S
import Grid
import Data.Either
import Debugging

type Rock = [Point]
type RockTower = S.Set Point
data Wind = L | R deriving (Show)
data FallingRock = FallingRock { rock :: Rock, settled :: Bool } deriving (Show)

data State = State { tower :: RockTower, wind :: [Wind] }

rocks :: [Rock]
rocks = [[(3, 0), (4, 0), (5, 0), (6, 0)]
       , [(4, 0), (3, 1), (4, 1), (5, 1), (4, 2)]
       , [(3, 0), (4, 0), (5, 0), (5, 1), (5, 2)]
       , [(3, 0), (3, 1), (3, 2), (3, 3)]
       , [(3, 0), (4, 0), (3, 1), (4, 1)]
       ]

moveViaWind :: Wind -> Point -> Point
moveViaWind w = addX translation
  where translation = case w of
                        L -> -1
                        R -> 1

dropOne :: Point -> Point
dropOne = addY (-1)

height :: RockTower -> Int
height = foldr (max . snd) 0 . S.elems

applyWind :: RockTower -> Rock -> Wind -> Rock
applyWind tower rock wind =
  let moved = map (moveViaWind wind) rock
      valid = all (\(x, _) -> 1 <= x && x <= 7) moved && all (`S.notMember` tower) moved
   in if valid then moved else rock

applyDrop :: RockTower -> Rock -> Rock
applyDrop tower rock =
  let moved = map dropOne rock
      valid = all (`S.notMember` tower) moved && all (\(_, y) -> 0 < y) moved
   in if valid then moved else rock

applyWindAndDrop :: RockTower -> Wind -> FallingRock -> FallingRock
applyWindAndDrop tower wind fallingRock =
  let windMoved = applyWind tower (rock fallingRock) wind
      dropped = applyDrop tower windMoved
      settled = dropped == windMoved
   in FallingRock {rock=dropped, settled=settled}

insertAll :: (Ord a) => [a] -> S.Set a -> S.Set a
insertAll toAdd s = foldl (flip S.insert) s toAdd

addRock :: State -> Rock -> State
addRock state rockToAdd =
  let currentTower = tower state
      currentHeight = height (tower state)
      startingState = (FallingRock {rock=map (addY (currentHeight + 4)) rockToAdd, settled=False}, wind state)
      applyNextWindAndDrop fallingState =
        let currentWind = head $ snd fallingState
            nextWind = tail $ snd fallingState
         in (applyWindAndDrop currentTower currentWind (fst fallingState), nextWind)
      (endRock, endWind) = until (settled . fst) applyNextWindAndDrop startingState
   in State { tower=insertAll (rock endRock) currentTower, wind=endWind }

parseInput :: String -> [Wind]
parseInput = cycle . map parseWind
  where
     parseWind x = case x of
                      '<' -> L
                      '>' -> R


totals :: [[Int]] -> [Int]
totals = map sum

--part1 :: String -> Int
part1 input =
  let initialState = State { tower = S.empty, wind=parseInput input }
      rocks' = take 2022 $ cycle rocks
   in height . tower . foldl addRock initialState $ rocks'
--   in 1
--   in tower . foldl addRock initialState $ rocks'

part2 :: String -> Int
part2 input =
  let initialState = State { tower = S.empty, wind=parseInput input }
      rocks' = take 100000 $ cycle rocks
   in height . tower . foldl addRock initialState $ rocks'

main :: IO ()
main = solve "17" part1 part2
