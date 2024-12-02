module Main where

import Data.List ( sort, transpose )

parseInput :: [String] -> [[Int]]
parseInput = map sort . transpose . map (map read . words)

partOne :: [[Int]] -> Int
partOne = sum . map (\[x, y] -> abs (x - y)) . transpose

partTwo :: [[Int]] -> Int
partTwo [_, []] = 0
partTwo [[], _] = 0
partTwo [lh : ls, rh : rs]
  | lh == rh = partTwo [ls, rh : rs] + sum (takeWhile (== lh) (rh : rs))
  | lh > rh = partTwo [lh : ls, rs]
  | rh > lh = partTwo [ls, rh : rs]

main :: IO ()
main = do
  input <- parseInput . lines <$> readFile "inputs/01.txt"
  print $ partOne input
  print $ partTwo input
