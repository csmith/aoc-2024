module Main where

import Data.Map (Map, empty, member, (!))
import Data.MemoUgly (memo)

type Value = Int

type Blinks = Int

type Cache = Map (Value, Blinks) Int

numbers :: [String] -> [Value]
numbers = map read

evaluate :: Blinks -> Value -> Int
evaluate = curry $ memo $ uncurry evaluate'
  where
    evaluate' blinks value
      | blinks == 0 = 1
      | otherwise = sum $ map (evaluate (blinks - 1)) (step value)

step :: Value -> [Value]
step x
  | x == 0 = [1]
  | even digits = [fst dumbTuple, snd dumbTuple]
  | otherwise = [2024 * x]
  where
    digits :: Int
    digits = ceiling (logBase 10 (fromIntegral (x + 1)))
    dumbTuple = divMod x (10 ^ (digits `div` 2))

main :: IO ()
main = do
  input <- numbers . words <$> readFile "inputs/11.txt"
  let cache = empty
  print $ sum $ map (evaluate 25) input
  print $ sum $ map (evaluate 75) input