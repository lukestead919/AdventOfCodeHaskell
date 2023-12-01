import Data.List (nub, isPrefixOf)
import Utils
import Control.Arrow(first)

type Path = String
type File = (String, Int)
type Parsing = (Path, [File])

upADirectory :: String -> String
upADirectory = reverse . dropWhile (/= '/') . tail . reverse

allDirs :: String -> [String]
allDirs path
  | '/' `elem` path = upADirectory path:allDirs (upADirectory path)
  | otherwise = []

handleCommand :: String -> Path -> Path
handleCommand ('c':'d':' ':xs) path = case xs of
                                    "/" -> "/"
                                    ".." -> upADirectory path
                                    x -> path ++ x ++ ['/']
handleCommand _ path = path

handleFile :: String -> Parsing -> [File]
handleFile ('d':'i':'r':' ':_) parsing = snd parsing
handleFile fileInfo parsing = let (size, fileName) = toPair . words $ fileInfo
  in snd parsing ++ [(fst parsing ++ fileName, read size)]

parseLine :: String -> Parsing -> Parsing
parseLine ('$':' ':xs) parsing = (handleCommand xs (fst parsing), snd parsing)
parseLine file parsing = (fst parsing, handleFile file parsing)

parseInput :: String -> [File]
parseInput = snd . foldl (flip parseLine) ("/", []) . lines

directorySize :: String -> [File] -> Int
directorySize dir = sumBy snd . filter (isPrefixOf dir . fst)

directories :: [File] -> [String]
directories x = nub . concatMap allDirs $ fileNames
                where fileNames = map fst x

part1 :: String -> Int
part1 x = let files = parseInput x
              dirs = directories files
              directorySizes = map (`directorySize` files) dirs
          in sum . filter (<=100000) $ directorySizes

part2 :: String -> Int
part2 x = let files = parseInput x
              dirs = directories files
              directorySizes = map (`directorySize` files) dirs
              spaceRequired = directorySize "/" files - 40000000
              canDelete = filter (>spaceRequired) directorySizes
           in minimum canDelete


main :: IO ()
main = solve "07" part1 part2
