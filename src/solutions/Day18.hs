import Data.List (sort, tails)
import qualified Data.Set as S
import qualified Data.Map as M
import Utils
import Debugging

type Point3D = (Int, Int, Int)

type Lava = S.Set Point3D

data Bounds = Bounds {minX :: Int, maxX :: Int, minY :: Int, maxY :: Int, minZ :: Int, maxZ :: Int} deriving (Show)

data Object = Lava | Exposed | Internal

type ObjectMap = M.Map Point3D Object

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

parseInput :: String -> S.Set Point3D
parseInput =
  let parseLine x = read ("(" ++ x ++ ")")
   in S.fromList . map parseLine . lines

neighbours :: Point3D -> [Point3D]
neighbours (x, y, z) =
  [ (x + 1, y, z),
    (x -1, y, z),
    (x, y + 1, z),
    (x, y -1, z),
    (x, y, z + 1),
    (x, y, z -1)
  ]

xCoord :: Point3D -> Int
xCoord (x, _, _) = x

yCoord :: Point3D -> Int
yCoord (_, y, _) = y

zCoord :: Point3D -> Int
zCoord (_, _, z) = z

bounds :: [Point3D] -> Bounds
bounds points = Bounds {minX = minX-1, maxX = maxX+1, minY = minY-1, maxY = maxY+1, minZ = minZ-1, maxZ = maxZ+1}
  where
    minX = minimum . map xCoord $ points
    maxX = maximum . map xCoord $ points
    minY = minimum . map yCoord $ points
    maxY = maximum . map yCoord $ points
    minZ = minimum . map zCoord $ points
    maxZ = maximum . map zCoord $ points

inBounds :: Bounds -> Point3D -> Bool
inBounds Bounds {minX, maxX, minY, maxY, minZ, maxZ} (x, y, z) =
  minX <= x
    && x <= maxX
    && minY <= y
    && y <= maxY
    && minZ <= z
    && z <= maxZ

touching :: Point3D -> Point3D -> Bool
touching p1 p2 = p2 `elem` neighbours p1

addAirPocket :: Bounds -> Point3D -> Lava -> Lava
addAirPocket bounds point lava
  | point `elem` lava = lava
  | otherwise = S.union lava newAirPocket
  where
    nextIteration airPocket =
      let airWithNeighbours = S.fromList $ concatMap (\c -> c : neighbours c) airPocket
          next = S.difference airWithNeighbours lava
       in if all (inBounds bounds) next then next else S.empty
    newAirPocket = until (\c -> nextIteration c == c) nextIteration $ S.singleton point

surfaceArea :: [Point3D] -> Int
surfaceArea shape = (6 * length shape) - (2 * (length . filter (uncurry touching) . pairs $ shape))

part1 :: String -> Int
part1 input =
  let points = parseInput input
   in surfaceArea $ S.elems points


--exposedPoints :: Bounds -> Lava -> S.Set Point3D
--exposedPoints bounds lava = exposedPoints
--  where
--    startingPoint = (minX bounds, minY bounds, minZ bounds)
--    startState = (S.singleton startingPoint, S.singleton startingPoint)
--    validPoint p = p `S.notMember` lava && inBounds bounds p
--    nextIteration state =
--      let neighbours' = S.fromList . filter validPoint . concatMap neighbours $ snd state
--          nowExposedPoints = S.union neighbours' (fst state)
--          newlyExposedPoints = S.difference neighbours' (fst state)
--
--       in (nowExposedPoints, traceLns newlyExposedPoints newlyExposedPoints)
--    exposedPoints = fst $ until (S.null . snd) nextIteration startState

part2 :: String -> Int
part2 input =
  let points = parseInput input
      lavaList = S.elems points
      bounds' = bounds lavaList
      neighbours' = concatMap neighbours lavaList
      addAirPocketAt' = addAirPocket bounds'
      lavaWithAir = foldr addAirPocketAt' points neighbours'
--      exposedPoints' = traceLns bounds' $ exposedPoints bounds' points
--   in traceLns exposedPoints' $ length . filter (`S.member` exposedPoints') $ neighbours'
   in surfaceArea $ S.elems lavaWithAir

main :: IO ()
main = solve "18" part1 part2
