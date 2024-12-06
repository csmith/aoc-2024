module Common where

import Data.Array.IArray

type Grid = Array (Int, Int) Char
type Coord = (Int, Int)
type Direction = (Int, Int)

outOfBounds :: Grid -> Coord -> Bool
outOfBounds grid position = x < minx || x > maxx || y < miny || y > maxy
  where
    ((minx, miny), (maxx, maxy)) = bounds grid
    (x, y) = position

parseGrid :: String -> Grid
parseGrid text = listArray ((0, 0), (numrows - 1, numcols - 1)) $ concat rows
  where
    rows = lines text
    numrows = length rows
    numcols = length $ head rows

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)