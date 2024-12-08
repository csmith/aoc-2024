module Main where

import Text.Regex.TDFA ((=~))

parseInput :: String -> [[String]]
parseInput s = s =~ "do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]

partOne :: [[String]] -> Int
partOne = sum . map process
  where
    -- Part one only cares about mul(), but we're feeding it the do and don'ts
    -- to avoid parsing the input multiple times. Just ignore them.
    process ["do()", _, _] = 0
    process ["don't()", _, _] = 0
    process [_, x, y] = read x * read y

partTwo :: [[String]] -> Int
partTwo = snd . foldl process (True, 0)
  where
    -- If we encounter a "do()" then enable future operations
    process (_, i) ["do()", _, _] = (True, i)
    -- A don't() disables future operations
    process (_, i) ["don't()", _, _] = (False, i)
    -- Otherwise it must be a mul(). If operations are enabled, add it
    process (True, i) [_, x, y] = (True, i + read x * read y)
    -- Must be a disabled mul, ignore it
    process r [_, _, _] = r

main :: IO ()
main = do
  input <- parseInput <$> readFile "inputs/03.txt"
  print $ partOne input
  print $ partTwo input
