module Main where

import Common (stripTrailingSpaces)
import Data.Char (digitToInt)
import Data.List (delete)
import Debug.Trace

type Id = Int

type Length = Int

type Position = Int

data Record = File Id Length | Space Length deriving (Eq, Show)

-- Accessing the ends of lists is O(n), so this whole thing is O(n^2), even though
-- the algorithm is pretty nice. Sad moo.
partOne :: Position -> [Record] -> Int
partOne _ [] = 0
partOne _ [Space _] = 0
partOne pos (File id length : xs) = checksum pos length id + partOne (pos + length) xs
partOne pos (Space length : xs) = case end of
  Space _ -> partOne pos (Space length : init xs)
  File fid flength
    -- We'll move all of the file here, and leave some space left over
    | flength < length -> partOne pos (File fid flength : Space (length - flength) : init xs)
    -- The file fits perfectly, no space left over
    | flength == length -> partOne pos (File fid flength : init xs)
    -- We'll move a bit of the file
    | flength >= length -> partOne pos ((File fid length : init xs) ++ [File fid (flength - length)])
  where
    end = last xs

partTwo :: Int -> [Record] -> [Record]
partTwo _ [] = []
partTwo _ [Space l] = [Space l]
partTwo max (Space l : xs) = Space l : partTwo max xs
partTwo max (File id length : xs)
  | id > max = File id length : partTwo max xs
  | null targets = File id length : partTwo max xs
  | targetLength == length = Space length : partTwo id (reverse $ replace target [File id length] (reverse xs))
  | otherwise = Space length : partTwo id (reverse $ replace target [File id length, Space (targetLength - length)] (reverse xs))
  where
    slength (Space l) = l
    targetLength = slength target
    target = head targets
    targets = reverse (filter fits xs)
    fits (File _ _) = False
    fits (Space l) = l >= length
    replace match target [] = []
    replace match target [x]
      | x == match = target
      | otherwise = [x]
    replace match target (x : xs)
      | x == match = target ++ xs
      | otherwise = x : replace match target xs

checksum :: Position -> Length -> Id -> Int
checksum pos length id = id * (length * pos + (length - 1) * length `div` 2)

checksumPartTwo :: [Record] -> Int
checksumPartTwo = sum . go 0
  where
    go :: Int -> [Record] -> [Int]
    go pos [] = []
    go pos [Space _] = [0]
    go pos [File id length] = [checksum pos length id]
    go pos ((Space l) : xs) = go (pos + l) xs
    go pos ((File id length) : xs) = checksum pos length id : go (pos + length) xs

parseInput :: String -> [Record]
parseInput = parseFile 0 . stripTrailingSpaces
  where
    parseFile _ [] = []
    parseFile id [x] = [File id (digitToInt x)]
    parseFile id (x : y : xs) = [File id (digitToInt x), Space (digitToInt y)] ++ parseFile (id + 1) xs

main :: IO ()
main = do
  l <- parseInput <$> readFile "inputs/09.txt"
  print $ partOne 0 l
  print $ checksumPartTwo $ reverse $ partTwo (length l) (reverse l)