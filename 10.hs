module Main where

import Common (Coord, Grid, add, outOfBounds, parseGrid, unique)
import Data.Array.IArray (assocs, (!))
import Data.Char (digitToInt)

cardinals :: [Coord]
cardinals = [(0, -1), (0, 1), (1, 0), (-1, 0)]

findPaths :: Grid -> [(Coord, [Coord])]
findPaths grid = map (\x -> (x, trace 0 x)) (starts grid)
  where
    starts = map fst . filter ((== '0') . snd) . assocs
    trace :: Int -> Coord -> [Coord]
    trace target coord
      | value == 9 = [coord]
      | otherwise = concatMap (trace (target + 1)) neighbours
      where
        value = digitToInt $ grid ! coord
        neighbours = filter ((== target + 1) . digitToInt . (grid !)) $ filter (not . outOfBounds grid) $ map (add coord) cardinals

partOne :: [(Coord, [Coord])] -> Int
partOne = sum . map (unique . snd)

partTwo :: [(Coord, [Coord])] -> Int
partTwo = sum . map (length . snd)

solve :: [(Coord, [Coord])] -> (Int, Int)
solve = foldl add (0, 0) . map (\(_, x) -> (unique x, length x))

main :: IO ()
main = do
  input <- findPaths . parseGrid <$> readFile "inputs/10.txt"
  let (partOne, partTwo) = solve input
  print partOne
  print partTwo