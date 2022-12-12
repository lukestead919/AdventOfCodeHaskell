module Grid
  ( Point,
    Grid,
    GridSize,
    neighbours,
    inGrid,
    gridValue,
    findValue,
    replaceInGrid
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
