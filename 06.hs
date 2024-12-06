module Main where

import Common (Coord, Direction, Grid, add, outOfBounds, parseGrid)
import Data.Array.IArray ( (!), assocs )
import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import Data.Set qualified as S

type Position = (Coord, Direction)

rotate :: Direction -> Direction
rotate (-1, 0) = (0, 1)
rotate (0, 1) = (1, 0)
rotate (1, 0) = (0, -1)
rotate (0, -1) = (-1, 0)

step :: Grid -> Coord -> Position -> Maybe Position
step grid obstacle (coord, dir)
  | outOfBounds grid next = Nothing
  | next == obstacle = Just (coord, rotate dir)
  | grid ! next == '#' = Just (coord, rotate dir)
  | otherwise = Just (next, dir)
  where
    next = add coord dir

patrol :: Grid -> Position -> [Position]
patrol grid pos
  | isNothing next = [pos]
  | otherwise = pos : patrol grid (fromJust next)
  where
    next = step grid (-1, -1) pos

loops :: Grid -> Coord -> Position -> Bool
loops = loop S.empty
  where
    loop :: S.Set Position -> Grid -> Coord -> Position -> Bool
    loop visited grid obstacle pos
      | S.member pos visited = True
      | isNothing next = False
      | otherwise = loop (S.insert pos visited) grid obstacle (fromJust next)
      where
        next = step grid obstacle pos

partOne :: [Position] -> Int
partOne = length . S.fromList . map fst

partTwo :: Grid -> [Position] -> Int
partTwo grid path = length $ S.fromList $ scan S.empty grid path
    where
    scan :: S.Set Coord -> Grid -> [Position] -> [Coord]
    scan _ _ [] = []
    scan _ _ [_] = []
    scan visited grid (pos:next:others)
        -- Just spinning on the spot, don't drop an obstacle on their head
        | currentCoord == nextCoord = scan visited grid (next:others)
        -- We've already travelled over the next cell, can't block it
        | S.member nextCoord visited = scan visited grid (next:others)
        -- Placing an obstacle in front of us causes a loop
        | loops grid nextCoord pos = nextCoord:scan (S.insert nextCoord visited) grid (next:others)
        -- Obstacle in front doesn't work
        | otherwise = scan (S.insert nextCoord visited) grid (next:others)
        where
            currentCoord = fst pos
            nextCoord = fst next


main :: IO ()
main = do
  input <- parseGrid <$> readFile "inputs/06.txt"
  let start = (fst $ fromJust $ find ((== '^') . snd) $ assocs input, (-1, 0))
  let path = patrol input start
  print $ partOne path
  print $ partTwo input path
