module Main where

import Common (add)

parseInput :: String -> (Int, [Int])
parseInput line = (read (takeWhile (/= ':') target), map read values)
  where
    (target : values) = words line

solve :: [(Int, [Int])] -> (Int, Int)
solve = foldl add (0, 0) . map evaluate

evaluate :: (Int, [Int]) -> (Int, Int)
evaluate (target, initial : values)
  | target `elem` partOneExpand [initial] values = (target, target)
  | target `elem` partTwoExpand [initial] values = (0, target)
  | otherwise = (0, 0)

partOneExpand :: [Int] -> [Int] -> [Int]
partOneExpand = foldl (\inputs value -> map (* value) inputs ++ map (+ value) inputs)

partTwoExpand :: [Int] -> [Int] -> [Int]
partTwoExpand = foldl (\inputs value -> map (* value) inputs ++ map (+ value) inputs ++ map (cc value) inputs)

cc :: Int -> Int -> Int
cc b a = a * 10 ^ digitCount b + b

digitCount :: Int -> Int
digitCount = go 1
  where
    go digits target = if target >= 10 then go (digits + 1) (div target 10) else digits

main :: IO ()
main = do
  input <- map parseInput . lines <$> readFile "inputs/07.txt"
  let (partOne, partTwo) = solve input
  print partOne
  print partTwo
