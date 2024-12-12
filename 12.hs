module Main where

import Common (CharGrid, Coord, Grid, add, neighbours, outOfBounds, parseGrid)
import Data.Array.IArray (Array, IArray (bounds), accum, array, assocs, indices, (!), (//))
import Data.List (groupBy, sortOn)
import Data.Set (Set)
import Data.Set qualified as Set

type Id = Int

region :: CharGrid -> Coord -> [Coord]
region = region' []
  where
    region' visited grid pos = foldl (`region'` grid) (pos : visited ++ next) next
      where
        next = filter unvisited $ filter same $ neighbours grid pos
        same x = grid ! x == grid ! pos
        unvisited x = x `notElem` visited

regions :: CharGrid -> [[Coord]]
regions grid = collate $ snd $ foldl check (0, empty) (indices grid)
  where
    collate = map (map fst) . groupBy (\a b -> snd a == snd b) . sortOn snd . assocs
    empty = array (bounds grid) (map (\(i, _) -> (i, -1)) (assocs grid))
    check :: (Id, Grid Id) -> Coord -> (Id, Grid Id)
    check (id, regions) pos
      -- This plot is already in a region
      | regions ! pos > -1 = (id, regions)
      -- Need to plot (heh) the region
      | otherwise =
          let regionCoords = region grid pos
              regionCoordsWithId = map (,id) regionCoords
              newRegions = accum (\_ y -> y) regions regionCoordsWithId
           in (id + 1, newRegions)

area :: [Coord] -> Int
area = length

perimeter :: CharGrid -> [Coord] -> Int
perimeter grid coords = sum $ map perimeter' coords
  where
    perimeter' coord = (4 -) $ length $ filter (== True) $ map sameRegion $ neighbours grid coord
      where
        sameRegion coord' = coord' `elem` coords

corners :: CharGrid -> Coord -> Int
corners grid pos = length $ filter (== True) (inners ++ outers)
  where
    -- A cell is an "outer" corner if it has different neighbours 90 degrees apart
    outers = [head n && n !! 2, n !! 2 && n !! 4, n !! 4 && n !! 6, n !! 6 && head n]
    -- A cell is an "inner" corner if it has same neighbours 90 degrees apart with a different neighbour inbetween
    inners = [not (head n) && not (n !! 2) && n !! 1, not (n !! 2) && not (n !! 4) && n !! 3, not (n !! 4) && not (n !! 6) && n !! 5, not (n !! 6) && not (head n) && n !! 7]
    target = grid ! pos
    n = map different [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
    different delta = outOfBounds grid (add pos delta) || (grid ! add pos delta) /= target

-- The number of sides is equal to the number of corners, which are much easier
-- to count.
sides :: CharGrid -> [Coord] -> Int
sides grid = sum . map (corners grid)

partOne :: CharGrid -> [[Coord]] -> Int
partOne grid regions = sum $ map score regions
  where
    score coords = area coords * perimeter grid coords

partTwo :: CharGrid -> [[Coord]] -> Int
partTwo grid regions = sum $ map score regions
  where
    score coords = area coords * sides grid coords

main :: IO ()
main = do
  grid <- parseGrid <$> readFile "inputs/12.txt"
  let r = regions grid
  print $ partOne grid r
  print $ partTwo grid r