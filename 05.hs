module Main where

import Data.List (elemIndex, sortBy, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import GHC.Generics ((:.:)(Comp1))

parseInput :: String -> ([[Int]], [[Int]])
parseInput x = (rules, updates)
  where
    halves = break null $ lines x
    rules :: [[Int]] = map (map read . splitOn "|") $ fst halves
    updates :: [[Int]] = map (map read . splitOn ",") $ tail $ snd halves

middle :: [Int] -> Int
middle x = x !! (length x `div` 2)

-- Pairs each update with a correctly sorted version of itself.
withSorted :: [[Int]] -> [[Int]] -> [([Int], [Int])]
withSorted rules = map (\x -> (x, sortBy ordering x))
    where
        ordering a b
            | [a,b] `elem` rules = LT
            | [b,a] `elem` rules = GT
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