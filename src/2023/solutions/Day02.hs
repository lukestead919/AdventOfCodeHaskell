import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List
import Data.List.Split
import Data.List.Utils (replace)
import Utils
import Debugging

data Cubes = Cubes { red :: Int, green :: Int, blue :: Int } deriving Show

parseInput :: String -> [[Cubes]]
parseInput = map (parseLine . last . splitOn ":") . lines
  where
    parseLine :: String -> [Cubes]
    parseLine = map (parseCube . lineToParsedVals) . splitOn ";"
    parseCube :: [[String]] -> Cubes
    parseCube = Cubes <$> findColour "red" <*> findColour "green" <*> findColour "blue"
    lineToParsedVals :: String -> [[String]]
    lineToParsedVals = map (splitOn " " . trim) . splitOn ","
    findColour :: String -> [[String]] -> Int
    findColour x = fromMaybe 0 . fmap read . fmap head . find ((\parsed -> last parsed == x))

maxCubes :: [Cubes] -> Cubes
maxCubes = foldl (\(Cubes r1 g1 b1) (Cubes r2 g2 b2) -> Cubes (max r1 r2) (max g1 g2) (max b1 b2)) (Cubes 0 0 0)

part1 :: String -> Int
part1 = part1' . parseInput
  where
    part1' = sum . mapWithIndex (\idx cubes -> if (isValid cubes) then idx + 1 else 0) . map maxCubes
    isValid :: Cubes -> Bool
    isValid (Cubes r g b) = r <= 12 && g <= 13 && b <= 14

part2 :: String -> Int
part2 = part2' . parseInput
  where
    part2' = sum . map (\(Cubes r g b) -> r * g * b) . map maxCubes

main :: IO ()
main = solve "02" part1 part2
