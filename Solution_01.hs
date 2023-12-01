-- For Advent of Code 2022
--
-- By Truman Collins
-- December 1, 2022

module Solution_01 (
  puzzle_01
) where

import Data.List
import Control.Monad


--
-- Code for Puzzle 1.
--

puzzle_01 :: IO (Int, Int)
puzzle_01 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  elfMealCalsLineStrs <- fmap lines (readFile "puzzle_01.inp")

  -- First group together all of the calorie counts for individual elves. Then convert the strings
  -- to ints and sum the calories per elf. This gives us a list of total calories per elf ordered as
  -- listed in the input. Next, get the three highest calorie counts in largest to smallest order.

  let elfMealCalsStrs = (filter (/= [[]]) . groupBy sameNullness) elfMealCalsLineStrs
      elfMealCalSums = map (sum . map read) elfMealCalsStrs
      threeHighestCal = (take 3 . sortBy (flip compare)) elfMealCalSums

  -- If this list isn't three long, then we didn't have enough elves to answer the problem.

  when (length threeHighestCal /= 3)
       (ioError $ userError "Too few elves to solve puzzle 1.")

  -- The first answer is just the highest calorie count of the three, which is the first. The second
  -- answer is the sum of all three.

  let maxCals1 = head threeHighestCal
      maxCals3 = sum threeHighestCal
      
  return (maxCals1, maxCals3)

-- Compare two strings and return true if either they are both null or non-null. Used to group
-- strings representing calories together in a list, as these are represented in the input
-- between empty lines.

sameNullness :: String -> String -> Bool
sameNullness xs ys = null xs == null ys
