-- Solutions to Advent of Code 2022.
--
-- By Truman Collins
-- December 1, 2022 to sometime in 2023.
--
-- Note that in the timings for the two answers for a particular puzzle. The first one includes
-- parsing, and common calculation, and any computation done before final error checks in the IO
-- code.

import Control.Monad
import Control.DeepSeq
import System.IO (hFlush, stdout)
import System.Clock
import Text.Printf
import Utilities
import Solution_01
import Solution_02
import Solution_03
import Solution_04
import Solution_05
import Solution_06
import Solution_07
import Solution_08
import Solution_09
import Solution_10
import Solution_11
import Solution_12
import Solution_13
import Solution_14
import Solution_15
import Solution_16
import Solution_17
import Solution_18
import Solution_19
import Solution_20
import Solution_21
import Solution_22
import Solution_23
import Solution_24
import Solution_25

-- The amount of space to allow for a result printed.

answerSpace :: Int
answerSpace = 20

-- The amount of space from the start of the line to the space after "Part 1:".

partSpace :: Int
partSpace = 18

-- Insure we can print strings without additional quotes.

newtype  NoQuotes = NoQuotes String
instance Show NoQuotes where show (NoQuotes str) = str

-- Helper functions for time calculations.

convertTimeToDouble :: TimeSpec -> Double
convertTimeToDouble tm = fromIntegral (sec tm) + fromIntegral (nsec tm) / 1.0e9

computeElapsedTime :: (TimeSpec, TimeSpec) -> Double
computeElapsedTime (startTime, endTime)
  = convertTimeToDouble endTime - convertTimeToDouble startTime

-- Check validity, based on known answer, and time running the solutions of two integer answers.

computeCheckAndPrint :: (PrintfArg a, PrintfArg b, Show a, Show b, Eq a, Eq b, NFData a, NFData b)
                        => IO (a, b) -> Int -> Bool -> (a, b) -> IO ()
computeCheckAndPrint puzzleFn puzzleNumber printPart2 (correctAnsA, correctAnsB) = do

  -- Time the parse, computation of part 1, and computation of part2. This is accomplished by
  -- triggering evaluation of the answers at appropriate times. We use deepseq because the answer
  -- might be a string and we want to insure that the entire string is computed.

  startParseTime <- getTime Realtime
  (resultA, resultB) <- puzzleFn
  endParseTime <- getTime Realtime
  let startTimeA = endParseTime
  endTimeA   <- resultA `deepseq` getTime Realtime
  let startTimeB = endTimeA
  endTimeB   <- resultB `deepseq` getTime Realtime
  
  let [diffP, diffA, diffB] = map computeElapsedTime [(startParseTime, endParseTime),
                                                      (startTimeA, endTimeA),
                                                      (startTimeB, endTimeB)]
      [diffStrP, diffStrA, diffStrB] = map (printf "%0.5f sec") [diffP, diffA, diffB]
      resultStrA = genResultString (resultA, correctAnsA, diffStrA)
      resultStrB = genResultString (resultB, correctAnsB, diffStrB)
      puzzleNumStr = padFront 3 (show puzzleNumber)
  
  -- Print out the results for parts A and B.

  putStr   $ mconcat ["Puzzle", puzzleNumStr]
  putStrLn $ mconcat [" Parse time: ", padFront (answerSpace - 3) " ", "(", diffStrP, ")"]
  hFlush stdout

  printResults partSpace "Part 1: " resultStrA
  when printPart2
    (printResults partSpace "Part 2: " resultStrB)

  where

    -- Print the results and flush the stream.

    printResults :: Int -> String -> String -> IO ()
    printResults padSize str res = do
      putStrLn $ mconcat [padFront padSize str, res]
      hFlush stdout

    -- Generate the resulting string from the result, correct answer and time difference.

    genResultString (result, corrAns, tDiffStr) = resultStr
      where
        resultStr = if result == corrAns
                    then ansPadded ++ " (" ++ tDiffStr ++ ")"
                    else "Error: expected " ++ show corrAns ++ ", but computed "
                         ++ show result ++ " (" ++ tDiffStr ++ ")"
        ansPadded = padFront answerSpace (removeQuotes ansStr)
        ansStr    = show result

    -- Remove the quote marks from the front and back if this string has them.

    removeQuotes :: String -> String
    removeQuotes [] = []
    removeQuotes [x] = [x]
    removeQuotes str@(x : xs)
      | x /= '"' = str
      | last xs == '"' = init xs
      | otherwise = str

--
-- Functions for the individual puzzles.
--

main :: IO ()
main = do

  startTime <- getTime Realtime

  -- Generate the results for each problem and check for the expected answer. The result will
  -- contain not only the result, but the time taken to compute it.

  computeCheckAndPrint puzzle_01  1 True (67622, 201491)
  computeCheckAndPrint puzzle_02  2 True (11666, 12767)
  computeCheckAndPrint puzzle_03  3 True (7850, 2581)
  computeCheckAndPrint puzzle_04  4 True (448, 794)
  computeCheckAndPrint puzzle_05  5 True ("RLFNRTNFB", "MHQTLJRLB")
  computeCheckAndPrint puzzle_06  6 True (1647, 2447)
  computeCheckAndPrint puzzle_07  7 True (1350966, 6296435)
  computeCheckAndPrint puzzle_08  8 True (1849, 201600)
  computeCheckAndPrint puzzle_09  9 True (6486, 2678)
  computeCheckAndPrint puzzle_10 10 True (12560, "PLPAFBCL")
  computeCheckAndPrint puzzle_11 11 True (99840, 20683044837)
  computeCheckAndPrint puzzle_12 12 True (447, 446)
  computeCheckAndPrint puzzle_13 13 True (5003, 20280)
  computeCheckAndPrint puzzle_14 14 True (737, 28145)
  computeCheckAndPrint puzzle_15 15 True (5870800, 10908230916597)
  computeCheckAndPrint puzzle_16 16 True (1915, 2772)
  computeCheckAndPrint puzzle_17 17 True (3163, 1560932944615)
  computeCheckAndPrint puzzle_18 18 True (4322, 2516)
  computeCheckAndPrint puzzle_19 19 True (1356, 27720)
  computeCheckAndPrint puzzle_20 20 True (10831, 6420481789383)
  computeCheckAndPrint puzzle_21 21 True (49288254556480, 3558714869436)
  computeCheckAndPrint puzzle_22 22 True (1428, 142380)
  computeCheckAndPrint puzzle_23 23 True (4091, 1036)
  computeCheckAndPrint puzzle_24 24 True (249, 735)
  computeCheckAndPrint puzzle_25 25 False ("2=0-2-1-0=20-01-2-20", 0)
  
  -- Report on the time taken by all of the solutions together.

  endTime <- getTime Realtime
  let diff = computeElapsedTime (startTime, endTime)
      diffStr = printf "%0.5f sec" diff

  putStrLn $ mconcat ["\nTotal time for all solutions: ", diffStr]
