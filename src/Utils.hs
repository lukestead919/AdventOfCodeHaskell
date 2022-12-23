module Utils
  ( solve,
    sumBy,
    toPair,
    replace,
    trim,
    windows,
    takeUntil,
    slice,
    zipWithNext,
    pairs,
    mapWithIndex,
    iterateTimes,
  )
where

import Data.Char (isSpace)
import Data.List (tails, transpose)
import Data.Traversable ()
import System.IO
import System.TimeIt

solve :: (Show ans1, Show ans2) => String -> (String -> ans1) -> (String -> ans2) -> IO ()
solve q p1 p2 = do
  let inputFile = "src/inputs/Day" ++ q ++ ".txt"
  handle <- openFile inputFile ReadMode
  contents <- hGetContents handle
  timeIt
    ( do
        putStrLn ("Part 1: " ++ show (p1 contents))
        putStrLn ("Part 2: " ++ show (p2 contents))
    )

  hClose handle

sumBy :: Num a => (b -> a) -> [b] -> a
sumBy f = sum . map f

toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)
toPair x = error ("not a pair, length = " ++ show (length x))

replace :: Int -> a -> [a] -> [a]
replace i e xs = case splitAt i xs of
  (before, _ : after) -> before ++ e : after
  _ -> xs

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

windows :: Int -> [a] -> [[a]]
windows m = transpose . take m . tails

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x : xs)
  | f x = x : takeUntil f xs
  | otherwise = [x]

slice :: Int -> Int -> [a] -> [a]
slice begin end = take (end - begin) . drop begin

zipWithNext :: [a] -> [(a, a)]
zipWithNext x = zip x (tail x)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

iterateTimes :: Int -> (a -> a) -> a -> a
iterateTimes times f x = (!! times) $ iterate f x
