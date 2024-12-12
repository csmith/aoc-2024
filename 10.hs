module Main where

import Common (Coord, CharGrid, add, parseGrid, unique, neighbours)
import Data.Array.IArray (assocs, (!))
import Data.Char (digitToInt)

findPaths :: CharGrid -> [(Coord, [Coord])]
findPaths grid = map (\x -> (x, trace 0 x)) (starts grid)
  where
    starts = map fst . filter ((== '0') . snd) . assocs
    trace :: Int -> Coord -> [Coord]
    trace target coord
      | value == 9 = [coord]
      | otherwise = concatMap (trace (target + 1)) n
      where
        value = digitToInt $ grid ! coord
        n = filter ((== target + 1) . digitToInt . (grid !)) $ neighbours grid coord

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