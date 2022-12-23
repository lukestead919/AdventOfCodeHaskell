module Grid
  ( Point,
    Grid,
    GridSize,
    neighbours,
    inGrid,
    gridValue,
    findValue,
    replaceInGrid,
    getPointsBetween,
    manhattanDistance,
    addPoints,
    add,
    addX,
    addY
  )
where

import Data.List (findIndex, elemIndex)
import Data.Maybe
  
type Point = (Int, Int)
type Grid a = [[a]]
type GridSize = (Int, Int)

gridSize :: Grid a -> GridSize
gridSize grid = (length (head grid), length grid)

neighbours :: Grid a -> Point -> [Point]
neighbours grid (x, y) = filter (`inGrid` grid) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

inGrid :: Point -> Grid a -> Bool
inGrid (x, y) grid = let (sizex, sizey) = gridSize grid
                      in 0 <= x && x < sizex && 0 <= y && y < sizey
                      
gridValue :: Grid a -> Point -> a
gridValue grid (x, y) = grid !! y !! x

findValue :: (Eq a) => Grid a -> a -> Maybe Point
findValue grid a = let found = map (elemIndex a) grid
                       x = findIndex isJust found
                    in fmap (\c -> (fromJust (found !! c), c)) x
                    
                    
replaceInGrid :: (Eq a) => a -> a -> Grid a -> Grid a
replaceInGrid a b = map (map (\c -> if a==c then b else c))

getPointsBetween :: Point -> Point -> [Point]
getPointsBetween (x1, y1) (x2, y2) = 
  [(x, y) | x <- [xMin..xMax], y <- [yMin..yMax]]
  where xMax = max x1 x2
        xMin = min x1 x2
        yMin = min y1 y2
        yMax = max y1 y2
        
addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1+x2, y1+y2)
        
add :: Int -> Int -> Point -> Point
add x y (a, b) = (a+x, b+y)

addX :: Int -> Point -> Point
addX = flip add 0

addY :: Int -> Point -> Point
addY = add 0 

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1) 