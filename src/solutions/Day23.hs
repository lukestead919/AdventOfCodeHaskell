import Data.List (delete, find, elemIndices, foldl', sort)
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Debugging
import Grid
import Utils

type Elves = S.Set Point

type ProposedMoves = M.Map Point Point

data State = State { elves :: Elves, turn :: Int, done :: Bool }

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show, Eq, Enum, Bounded, Ord)

allDirections :: [Direction]
allDirections = [minBound..maxBound]

next :: (Bounded a, Eq a, Enum a) => a -> a
next a
  | a == maxBound = minBound
  | otherwise = succ a

prev :: (Bounded a, Eq a, Enum a) => a -> a
prev a
  | a == minBound = maxBound
  | otherwise = pred a

withAdjacentDirections :: Direction -> [Direction]
withAdjacentDirections direction = [prev direction, direction, next direction]

directionToPoint :: Direction -> Point
directionToPoint d = case d of
  N  -> (0, -1)
  NE -> (1, -1)
  E  -> (1, 0)
  SE -> (1, 1)
  S  -> (0, 1)
  SW -> (-1, 1)
  W  -> (-1, 0)
  NW -> (-1, -1)

moveInDirection :: Direction -> Point -> Point
moveInDirection d = addPoints (directionToPoint d)

getDirectionsForTurn :: Int -> [Direction]
getDirectionsForTurn turnNum = take 4 . drop turnNum . cycle $ [N, S, W, E]

parseInput :: String -> Elves
parseInput = S.fromList . concat . mapWithIndex (\idx row -> map (, idx) ('#' `elemIndices` row)) . lines

getProposedMoves :: State -> ProposedMoves
getProposedMoves State { elves, turn } = proposed
  where
    directions = getDirectionsForTurn turn
    proposedMoveForElf elf = move
      where
        neighbourInDirection = M.fromSet (\d -> moveInDirection d elf `S.member` elves) (S.fromList allDirections)
        noNeighboursInDirections = not . any (\d -> M.findWithDefault False d neighbourInDirection)
        noNeighbours = noNeighboursInDirections allDirections
        validDirections = filter (noNeighboursInDirections . withAdjacentDirections) directions
        move = if noNeighbours || null validDirections then elf else moveInDirection (head validDirections) elf
    proposed = M.fromList . map (\elf -> (elf, proposedMoveForElf elf)) . S.elems $ elves

makeProposedMoves :: ProposedMoves -> Elves
makeProposedMoves proposed = moves
  where
    numberMovingToSpace = M.fromListWith (+) . flip zip (repeat 1) $ M.elems proposed
    actualMove (orig, proposed) = if M.findWithDefault 1 proposed numberMovingToSpace > 1 then orig else proposed
    moves = S.fromList . map actualMove . M.toList $ proposed

moveElves :: State -> State
moveElves state@State {elves, turn} = newState
  where
    proposed = getProposedMoves state
    moved = makeProposedMoves proposed
    newState = State {elves=moved, turn=turn + 1, done = moved == elves}

bounds :: [Point] -> (Point, Point)
bounds points = ((minX, minY), (maxX, maxY))
  where
    minX = minimum . map fst $ points
    maxX = maximum . map fst $ points
    minY = minimum . map snd $ points
    maxY = maximum . map snd $ points

part1 input = answer
  where
    startElves = parseInput input
    state = State {elves=startElves, turn=0, done=False}
    movedElves = S.toList $ elves $ iterateTimes 10 moveElves state
    answer = (length . uncurry getPointsBetween . bounds $ movedElves) - length movedElves
--    answer = movedElves

part2 :: String -> Int
part2 input = answer
  where
    startElves = parseInput input
    state = State {elves=startElves, turn=0, done=False}
    answer = turn $ until done moveElves state

main :: IO ()
main = solve "23" part1 part2
