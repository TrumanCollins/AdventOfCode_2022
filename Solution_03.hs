-- For Advent of Code 2022
--
-- By Truman Collins
-- December 3, 2022

module Solution_03 (
  puzzle_03
) where

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Bifunctor as BF
import qualified Data.Set as S
import Control.Monad


--
-- Code for Puzzle 3.
--

puzzle_03 :: IO (Int, Int)
puzzle_03 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap lines (readFile "puzzle_03.inp")

  -- Generate the sets of intersecting characters for each compartment. Make sure there is just one
  -- for each.

  let commonElementsSet = map (uncurry S.intersection . BF.bimap S.fromList S.fromList . splitInTwo)
                          puzzInp
      justOneInEach = all ((== 1) . S.size) commonElementsSet

  unless justOneInEach
         (ioError $ userError "Intersections contain other than 1 in puzzle 3.")

  -- Sum the priorities of the intersecting character for each rucksack.
  
  let priorities = map (priorityOfChar . S.elemAt 0) commonElementsSet
      prioritySumPart1 = sum priorities

  let prioritySumPart2M = sumTriplets 0 puzzInp

  when (isNothing prioritySumPart2M)
       (ioError $ userError "Problem computing triple priority sums.")

  let prioritySumPart2 = fromJust prioritySumPart2M

  return (prioritySumPart1, prioritySumPart2)

-- Return the priority value of the given character.

priorityOfChar :: Char -> Int
priorityOfChar ch
  | isLower ch = ord ch - 96
  | isUpper ch = ord ch - 38
  | otherwise = error "Bad char to priorityOfChar."

-- Split the string in half, and return the two halves in a pair.

splitInTwo :: String -> (String, String)
splitInTwo str = let len = length str
                 in  splitAt (len `quot` 2) str

-- Go through the list of strings by threes, finding the intersection of each three, which needs to
-- be a single character, and summing the priority values associated with them. If the total number
-- of strings is not divisible by 3 or there is not just a single intersecting character for each
-- triple, return Nothing.

sumTriplets :: Int -> [String] -> Maybe Int
sumTriplets accSum [] = Just accSum
sumTriplets accSum strs
  | length first3 /= 3 = Nothing
  | S.size intersectionSet /= 1 = Nothing
  | otherwise = let newSum = accSum + (priorityOfChar . S.elemAt 0) intersectionSet
                in  newSum `seq` sumTriplets newSum remaining
  where
    intersectionSet = foldl1' S.intersection (map S.fromList first3)
    (first3, remaining) = splitAt 3 strs
