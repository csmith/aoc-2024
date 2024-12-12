module Main where

import Common (Coord, Direction, CharGrid, add, count, outOfBounds, parseGrid)
import Data.Array.IArray (IArray (bounds), indices, (!))

directions :: [Direction]
directions = [(0, 1), (0, -1), (1, 1), (1, 0), (1, -1), (-1, 1), (-1, 0), (-1, -1)]

matches :: CharGrid -> String -> Coord -> Direction -> Bool
matches _ [] _ _ = True
matches grid (chr : rest) position direction
  | outOfBounds grid position = False
  | c /= chr = False
  | otherwise = matches grid rest (add position direction) direction
  where
    b = bounds grid
    c = grid ! position

partOne :: CharGrid -> Int
partOne grid = sum $ map scan (indices grid)
  where
    scan pos = count (== True) $ map (matches grid "XMAS" pos) directions

partTwo :: CharGrid -> Int
partTwo grid = count (== True) $ map scan (indices grid)
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

main :: IO ()
main = do
  input <- parseGrid <$> readFile "inputs/04.txt"
  print $ partOne input
  print $ partTwo input