module Main where

import Data.List (drop, intercalate)
import Data.List.Split (splitOn)
import Data.Bits (Bits(xor, shiftR))
import Debug.Trace

data Instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Enum, Eq, Ord, Show)

data Computer = Computer
  { ip :: Int,
    registerA :: Int,
    registerB :: Int,
    registerC :: Int,
    instructions :: [Instruction],
    output :: [Instruction],
    hcf :: Bool
  }
  deriving (Show)

parseInput :: [String] -> Computer
parseInput lines = Computer {
    ip = 0,
    registerA = read $ drop 12 $ head lines,
    registerB = read $ drop 12 $ lines !! 1,
    registerC = read $ drop 12 $ lines !! 2,
    instructions = map (toEnum . read) $ splitOn "," $ drop 9 $ lines !! 4,
    output = [],
    hcf = False
}

step :: Computer -> Computer
step c
    | ip c >= length (instructions c) = c { hcf = True }
    -- The adv instruction (opcode 0) performs division. The numerator is the value in the A register.
    -- The denominator is found by raising 2 to the power of the instruction's combo operand.
    -- The result of the division operation is truncated to an integer and then written to the A register.
    | instr == ADV = c { registerA = registerA c `shiftR` comboOperand, ip = ip' }
    -- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand,
    -- then stores the result in register B.
    | instr == BXL = c { registerB = xor (registerB c) literalOperand, ip = ip' }
    -- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its
    -- lowest 3 bits), then writes that value to the B register.
    | instr == BST = c { registerB = comboOperand `mod` 8, ip = ip' }
    -- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero,
    -- it jumps by setting the instruction pointer to the value of its literal operand; if this instruction jumps,
    -- the instruction pointer is not increased by 2 after this instruction.
    | instr == JNZ && registerA c == 0 = c { ip = ip' }
    | instr == JNZ = c { ip = literalOperand }
    -- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, then stores the result
    -- in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
    | instr == BXC = c { registerB = xor (registerB c) (registerC c), ip = ip' }
    -- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value. (If
    -- a program outputs multiple values, they are separated by commas.)
    | instr == OUT = c { output = output c ++ [toEnum $ comboOperand `mod` 8], ip = ip' }
    -- The bdv instruction (opcode 6) works exactly like the adv instruction except that the result is stored in the B
    -- register. (The numerator is still read from the A register.)
    | instr == BDV = c { registerB = registerA c `shiftR` comboOperand, ip = ip' }
    -- The cdv instruction (opcode 7) works exactly like the adv instruction except that the result is stored in the C
    -- register. (The numerator is still read from the A register.)
    | instr == CDV = c { registerC = registerA c `shiftR` comboOperand, ip = ip' }
    where
        ip' = 2 + ip c
        instr = instructions c !! ip c
        operand = instructions c !! (1 + ip c)
        literalOperand = fromEnum operand
        comboOperand = case literalOperand of
            -- Combo operands 0 through 3 represent literal values 0 through 3.
            0 -> 0
            1 -> 1
            2 -> 2
            3 -> 3
            -- Combo operand 4 represents the value of register A.
            4 -> registerA c
            -- Combo operand 5 represents the value of register B.
            5 -> registerB c
            -- Combo operand 6 represents the value of register C.
            6 -> registerC c

run :: Computer -> [Instruction]
run c
    | hcf c = output c
    | otherwise = run $ step c

main :: IO ()
main = do
  input <- parseInput . lines <$> readFile "inputs/17.txt"
  print $ intercalate "," $ map (show . fromEnum) $ run input
