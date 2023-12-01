-- For Advent of Code 2022
--
-- By Truman Collins
-- December 10, 2022

module Solution_10 (
  puzzle_10
) where

import Control.Applicative
import Control.Monad
import Parsers

data Instruction = Noop | AddX Int deriving (Eq, Show)
type InstrCount = (Instruction, Int)

--
-- Code for Puzzle 10.
--

puzzle_10 :: IO (Int, String)
puzzle_10 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile "puzzle_10.inp")

  when (badParseOfInputLines puzzInp)
       (ioError $ userError "Bad input for puzzle 10.")

  let instructionList = map (fst . head) puzzInp
      expInstrList :: [InstrCount]
      expInstrList = foldr expandInstr [] instructionList
      part1MonCycles = [20,60..220]
      xVals = scanl handleClockTick 1 expInstrList
      part1Ans = (sum . neededSignalStrengths 1 part1MonCycles) xVals

      spriteNumbers = (zipWith onOrOff xVals . zip [1..] . concat . replicate 6) [0..39]
      spriteLines = chopItUp 40 spriteNumbers
      correctAns2 = ["###..#....###...##..####.###...##..#....",
                     "#..#.#....#..#.#..#.#....#..#.#..#.#....",
                     "#..#.#....#..#.#..#.###..###..#....#....",
                     "###..#....###..####.#....#..#.#....#....",
                     "#....#....#....#..#.#....#..#.#..#.#....",
                     "#....####.#....#..#.#....###...##..####."]

  let part2Ans = if spriteLines /= correctAns2 then "Failed" else "PLPAFBCL"

  return (part1Ans, part2Ans)

-- Chop the string into chunks of the given size.

chopItUp :: Int -> [a] -> [[a]]
chopItUp _ [] = []
chopItUp chopLen xs = choppedFront : chopItUp chopLen choppedRemainder
  where
    (choppedFront, choppedRemainder) = splitAt chopLen xs

-- Return the character representing on or off determined by the sprite given and the overlap
-- allowed to turn that sprite on.

onOrOff :: Int -> (Int, Int) -> Char
onOrOff xVal (_cycle, spriteX)
  | spriteOverlap = '#'
  | otherwise = '.'
  where
    spriteOverlap = abs (xVal - spriteX) <= 1

neededSignalStrengths :: Int -> [Int] -> [Int] -> [Int]
neededSignalStrengths _ [] _ = []
neededSignalStrengths _ _ [] = []
neededSignalStrengths currCycle css@(c : cs) (x : xs)
  | currCycle == c = (c * x) : neededSignalStrengths nextCycle cs xs
  | otherwise = neededSignalStrengths nextCycle css xs
  where
    nextCycle = currCycle + 1

-- Used to expand each instruction to have n instances in the resulting list, where n is the number
-- of clock cycles it takes to operate, and pair with a count down to 1 for the last cycle.

expandInstr :: Instruction -> [InstrCount] -> [InstrCount]
expandInstr Noop acc = (Noop, 1) : acc
expandInstr addInstr@(AddX _) acc = (addInstr, 2) : (addInstr, 1) : acc

-- Take the current x value and the current instruction and count, and change the resulting x-value
-- if the operation is an AddX and it is finishing the last cycle in its process.

handleClockTick :: Int -> InstrCount -> Int
handleClockTick currVal (Noop, _) = currVal
handleClockTick currVal (AddX val, 1) = currVal + val
handleClockTick currVal (AddX _, _) = currVal

-- Parse an input line.

readInputLine :: Parser Instruction
readInputLine = do
      _ <- symbol "noop"
      return Noop
    <|> do
      _ <- symbol "addx"
      _ <- space
      AddX <$> int
