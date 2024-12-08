module Common where

import Data.Array.IArray (Array, IArray (bounds), listArray)
import Data.List (filter, subsequences)
import Data.Set (fromList)

type Grid = Array (Int, Int) Char

type Coord = (Int, Int)

type Direction = (Int, Int)

-- Checks if a given co-ordinate lies outside the bounds of the grid.
outOfBounds :: Grid -> Coord -> Bool
outOfBounds grid position = x < minx || x > maxx || y < miny || y > maxy
  where
    ((minx, miny), (maxx, maxy)) = bounds grid
    (x, y) = position

-- Parses an ASCII grid into an array.
parseGrid :: String -> Grid
parseGrid text = listArray ((0, 0), (numrows - 1, numcols - 1)) $ concat rows
  where
    rows = lines text
    numrows = length rows
    numcols = length $ head rows

-- Adds two tuples together.
add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Subtracts a tuple from another.
sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Counts the number of unique elements in a list.
unique :: Ord a => [a] -> Int
unique = length . fromList

-- Counts the number of matching elements in a list.
count :: Ord a => (a -> Bool) -> [a] -> Int
count f x = length $ filter f x

-- Returns all possible combinations of length k of the given elements.
combinations :: Int -> [a] -> [[a]]
combinations k = filter ((k ==) . length) . subsequences
