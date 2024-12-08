module Main where

import Common (Coord, add, combinations, sub, unique)
import Data.List (concatMap, filter)
import Data.Map (Map, elems, empty, singleton, unionWith)

parseInput :: [String] -> Map Char [Coord]
parseInput = parseLines 0
  where
    parseLines _ [] = empty
    parseLines y (line : lines) = unionWith (++) (parseLines (y + 1) lines) (parseLine y 0 line)
    parseLine _ _ [] = empty
    parseLine y x (c : cs)
      | c == '.' = parseLine y (x + 1) cs
      | otherwise = unionWith (++) (singleton c [(y, x)]) (parseLine y (x + 1) cs)

partOne :: (Int, Int) -> [[Coord]] -> Int
partOne bounds coords = unique $ filter (inBounds bounds) $ concatMap antinodes (pairs coords)
  where
    antinodes [x, y] = [sub y diff, add x diff]
      where
        diff = sub x y

partTwo :: (Int, Int) -> [[Coord]] -> Int
partTwo bounds coords = unique $ concatMap antinodes (pairs coords)
  where
    antinodes [x, y] = [x] ++ repeatedly sub x diff ++ repeatedly add x diff
      where
        diff = sub x y
    repeatedly :: ((Int, Int) -> (Int, Int) -> (Int, Int)) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
    repeatedly op val delta
      | inBounds bounds val = val : repeatedly op (op val delta) delta
      | otherwise = []

pairs :: [[Coord]] -> [[Coord]]
pairs = concatMap (combinations 2)

inBounds :: (Int, Int) -> Coord -> Bool
inBounds (width, height) (x, y) = x >= 0 && y >= 0 && x < width && y < height

main :: IO ()
main = do
  l <- lines <$> readFile "inputs/08.txt"
  let input = elems $ parseInput l
  let dimens = (length $ head l, length l)
  print $ partOne dimens input
  print $ partTwo dimens input