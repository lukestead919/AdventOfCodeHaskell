import qualified Data.Set as S
import Utils

type Point3D = (Int, Int, Int)

type Lava = S.Set Point3D

data Bounds = Bounds {minX :: Int, maxX :: Int, minY :: Int, maxY :: Int, minZ :: Int, maxZ :: Int} deriving (Show)

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
bounds points = Bounds {minX = minX -1, maxX = maxX + 1, minY = minY -1, maxY = maxY + 1, minZ = minZ -1, maxZ = maxZ + 1}
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

exposedPointsPartA :: Bounds -> Lava -> S.Set Point3D
exposedPointsPartA Bounds {minX, maxX, minY, maxY, minZ, maxZ} lava = air
  where
    pointsInBounds = S.fromList $ [(x, y, z) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ]]
    air = S.difference pointsInBounds lava

exposedPointsPartB :: Bounds -> Lava -> S.Set Point3D
exposedPointsPartB bounds lava = exposedPoints
  where
    startingPoint = (minX bounds, minY bounds, minZ bounds)
    startState = (S.singleton startingPoint, S.singleton startingPoint)
    validPoint p = p `S.notMember` lava && inBounds bounds p
    nextIteration state =
      let neighbours' = S.fromList . filter validPoint . concatMap neighbours $ snd state
          nowExposedPoints = S.union neighbours' (fst state)
          newlyExposedPoints = S.difference neighbours' (fst state)
       in (nowExposedPoints, newlyExposedPoints)
    exposedPoints = fst $ until (S.null . snd) nextIteration startState

solveForPart :: (Bounds -> Lava -> S.Set Point3D) -> String -> Int
solveForPart getExposedPoints input =
  let lava = parseInput input
      lavaList = S.elems lava
      bounds' = bounds lavaList
      neighbours' = concatMap neighbours lavaList
      exposedPoints' = getExposedPoints bounds' lava
   in length . filter (`S.member` exposedPoints') $ neighbours'

part1 :: String -> Int
part1 = solveForPart exposedPointsPartA

part2 :: String -> Int
part2 = solveForPart exposedPointsPartB

main :: IO ()
main = solve "18" part1 part2
