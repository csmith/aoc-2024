module Main where

import Common (stripTrailingSpaces)
import Data.Char (digitToInt)
import Data.List (delete)

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

-- Doesn't actually solve part two. Instead does basically the same as part one,
-- but moves whole files. Yay poorly worded instructions.
partTwo :: Position -> [Record] -> Int
partTwo _ [] = 0
partTwo _ [Space _] = 0
partTwo pos [File id length] = checksum pos length id
partTwo pos (File id length : xs) = checksum pos length id + partTwo (pos + length) xs
partTwo pos (Space length : xs)
  | length == 0 = partTwo pos xs
  | null matches = partTwo (pos + length) xs
  | otherwise = partTwo pos ([match, Space (length - matchLength)] ++ delete match xs)
  where
    shortEnough length (File _ f1) = f1 <= length
    shortEnough _ (Space _) = False
    matches = reverse $ filter (shortEnough length) xs
    match = head matches
    fileLength (File _ l) = l
    matchLength = fileLength match

checksum :: Position -> Length -> Id -> Int
checksum pos length id = id * (length * pos + (length - 1) * length `div` 2)

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
  print $ partTwo 0 l