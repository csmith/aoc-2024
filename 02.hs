module Main where

import Common (count)

parseInput :: [String] -> [[Int]]
parseInput = map (map read . words)

partOne :: [[Int]] -> Int
partOne = count safe

safe :: [Int] -> Bool
safe = valid . diffs . pairs
  where
    pairs x = zip (tail x) x
    diffs = map (uncurry (-))
    valid x = all (\d -> d >= 1 && d <= 3) x || all (\d -> d >= -3 && d <= -1) x

partTwo :: [[Int]] -> Int
partTwo = count (any safe . dampened)

dampened :: [Int] -> [[Int]]
dampened [] = []
dampened [x] = [[x], []]
dampened (x : xs) = xs : map (x :) (dampened xs)

main :: IO ()
main = do
  input <- parseInput . lines <$> readFile "inputs/02.txt"
  print $ partOne input
  print $ partTwo input
