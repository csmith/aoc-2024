module Main where

import Data.Array.IArray

type Grid = Array (Int, Int) Char

directions :: [(Int, Int)]
directions = [(0, 1), (0, -1), (1, 1), (1, 0), (1, -1), (-1, 1), (-1, 0), (-1, -1)]

outOfBounds :: Grid -> (Int, Int) -> Bool
outOfBounds grid position = x < minx || x > maxx || y < miny || y > maxy
  where
    ((minx, miny), (maxx, maxy)) = bounds grid
    (x, y) = position

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

matches :: Grid -> String -> (Int, Int) -> (Int, Int) -> Bool
matches _ [] _ _ = True
matches grid (chr : rest) position direction
  | outOfBounds grid position = False
  | c /= chr = False
  | otherwise = matches grid rest (add position direction) direction
  where
    b = bounds grid
    c = grid ! position

partOne :: Grid -> Int
partOne grid = sum $ map scan (indices grid)
  where
    scan pos = length $ filter (== True) $ map (matches grid "XMAS" pos) directions

partTwo :: Grid -> Int
partTwo grid = length $ filter (== True) $ map scan (indices grid)
  where
    scan pos
        | at pos /= 'A' = False
        | not $ valid $ diagonal pos 1 = False
        | not $ valid $ diagonal pos (-1) = False
        | otherwise = True
    diagonal pos x = [at (add pos (x, -1)), at (add pos (-x, 1))]
    valid [a, b] = (a == 'M' || a == 'S') && (b == 'M' || b == 'S') && b /= a
    at pos
        | outOfBounds grid pos = '.'
        | otherwise = grid ! pos

parseInput :: String -> Grid
parseInput text = listArray ((0, 0), (numrows - 1, numcols - 1)) $ concat rows
  where
    rows = lines text
    numrows = length rows
    numcols = length $ head rows

main :: IO ()
main = do
  input <- parseInput <$> readFile "inputs/04.txt"
  print $ partOne input
  print $ partTwo input