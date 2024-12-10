module Main where

import Data.Array.IArray (assocs, (!))
import Common (Coord, Grid, add, outOfBounds, parseGrid, unique)
import Data.Char (digitToInt)

cardinals :: [Coord]
cardinals = [(0, -1), (0, 1), (1, 0), (-1, 0)]

findPaths :: Grid -> [(Coord, [Coord])]
findPaths grid = map (\x -> (x, trace 0 x)) (starts grid)
    where
        starts = map fst . filter (\(_, v) -> v == '0') . assocs
        trace :: Int -> Coord -> [Coord]
        trace target coord
            | value /= target = []
            | value == 9 = [coord]
            | otherwise = concatMap (trace (target + 1)) neighbours
            where
                value = digitToInt $ grid ! coord
                neighbours = filter (not . outOfBounds grid) $ map (add coord) cardinals

partOne :: [(Coord, [Coord])] -> Int
partOne = sum . map (unique . snd)

partTwo :: [(Coord, [Coord])] -> Int
partTwo = sum . map (length . snd)

main :: IO ()
main = do
  input <- findPaths . parseGrid <$> readFile "inputs/10.txt"
  print $ partOne input
  print $ partTwo input