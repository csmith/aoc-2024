module Main where

parseInput :: [String] -> [[Int]]
parseInput = map (map read . words)

partOne :: [[Int]] -> Int
partOne x = length $ filter safe x

safe :: [Int] -> Bool
safe x = valid $ diffs $ pairs x
  where
    pairs x = zip (tail x) x
    diffs = map (uncurry (-))
    valid x = all (\d -> d >= 1 && d <= 3) x || all (\d -> d >= -3 && d <= -1) x

partTwo :: [[Int]] -> Int
partTwo x = length $ filter (any safe . dampened) x

dampened :: [Int] -> [[Int]]
dampened [] = []
dampened [x] = [[x], []]
dampened (x : xs) = xs : map (x :) (dampened xs)

main :: IO ()
main = do
  input <- parseInput . lines <$> readFile "inputs/02.txt"
  print $ partOne input
  print $ partTwo input
