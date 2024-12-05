module Main where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Set (fromList, member, Set)

parseInput :: String -> (Set (Int, Int), [[Int]])
parseInput x = (rulesMap, updates)
  where
    halves = break null $ lines x
    rules :: [[Int]] = map (map read . splitOn "|") $ fst halves
    rulesMap = fromList $ map (\x -> (head x, x !! 1)) rules
    updates :: [[Int]] = map (map read . splitOn ",") $ tail $ snd halves

middle :: [Int] -> Int
middle x = x !! (length x `div` 2)

-- Pairs each update with a correctly sorted version of itself.
withSorted :: Set (Int, Int) -> [[Int]] -> [([Int], [Int])]
withSorted rules = map (\x -> (x, sortBy ordering x))
    where
        ordering a b
            | member (a,b) rules = LT
            | member (b,a) rules = GT
            | otherwise = EQ

partOne :: [([Int], [Int])] -> Int
partOne = sum . map value
    where
        value (left, right)
            | left == right = middle left
            | otherwise = 0

partTwo :: [([Int], [Int])] -> Int
partTwo = sum . map value
    where
        value (left, right)
            | left /= right = middle right
            | otherwise = 0

main :: IO ()
main = do
  sorted <- uncurry withSorted . parseInput <$> readFile "inputs/05.txt"
  print $ partOne sorted
  print $ partTwo sorted