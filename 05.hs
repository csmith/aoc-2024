module Main where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)

parseInput :: String -> (Set (String, String), [[String]])
parseInput x = (rules, updates)
  where
    halves = break null $ lines x
    rules = fromList $ map (\x -> (take 2 x, drop 3 x)) (fst halves)
    updates = map (splitOn ",") $ tail $ snd halves

solve :: Set (String, String) -> [[String]] -> (Int, Int)
solve rules = foldl check (0, 0)
  where
    check (p1, p2) original
      | original /= sorted = (p1, p2 + read (mid sorted))
      | otherwise = (p1 + read (mid original), p2)
      where
        mid x = x !! (length x `div` 2)
        sorted = sortBy ordering original
        ordering a b
          | member (a, b) rules = LT
          | otherwise = GT

main :: IO ()
main = do
  (partOne, partTwo) <- uncurry solve . parseInput <$> readFile "inputs/05.txt"
  print partOne
  print partTwo