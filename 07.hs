module Main where

parseInput :: String -> (Int, [Int])
parseInput line = (read (takeWhile (/= ':') target), map read values)
  where
    (target : values) = words line

partOne :: [(Int, [Int])] -> Int
partOne lines = sum $ map (evaluate partOneExpand) lines

partTwo :: [(Int, [Int])] -> Int
partTwo lines = sum $ map (evaluate partTwoExpand) lines

evaluate :: ([Int] -> [Int] -> [Int]) -> (Int, [Int]) -> Int
evaluate expand (target, initial : values)
  | target `elem` expand [initial] values = target
  | otherwise = 0

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
  print $ partOne input
  print $ partTwo input
