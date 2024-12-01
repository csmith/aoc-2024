module Main where

import Data.List

parseLine :: String -> [Int]
parseLine = map read . words

parseInput :: [String] -> [[Int]]
parseInput = map sort . transpose . map parseLine

diff :: [Int] -> Int
diff (x:y:[]) = abs (x - y)

partOne :: [[Int]] -> Int
partOne = sum . map diff . transpose

matches :: ([Int], [Int]) -> [Int]
matches (_, []) = []
matches ([], _) = []
matches (lh:ls, rh:rs)
    | lh == rh = (takeWhile (==lh) (rh:rs)) ++ (matches (ls, rh:rs))
    | lh > rh = matches (lh:ls, rs)
    | rh > lh = matches (ls, rh:rs)

partTwo :: [[Int]] -> Int
partTwo (l:r:[]) = sum $ matches (l, r)

main = do
    input <- parseInput <$> lines <$> (readFile "inputs/01.txt")
    print $ partOne input
    print $ partTwo input
