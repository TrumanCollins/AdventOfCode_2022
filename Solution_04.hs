-- For Advent of Code 2022
--
-- By Truman Collins
-- December 4, 2022

module Solution_04 (
  puzzle_04
) where

import Control.Applicative
import Control.Monad
import Parsers


--
-- Code for Puzzle 4.
--

-- Types defined for ranges, range pairs, and relationships between range pairs.

type SingleRange = (Int, Int)
type RangePair = (SingleRange, SingleRange)
data RangeRelationship = BadRange1 | BadRange2 | Disjoint1LowerThan2
                         | Disjoint2LowerThan1 | Contained1In2 | Contained2In1
                         | Overlap1LowerThan2 | Overlap2LowerThan1 deriving (Show, Eq)

puzzle_04 :: IO (Int, Int)
puzzle_04 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  parseResult <- fmap (parse readRangePairList) (readFile "puzzle_04.inp")

  -- Make sure there was a valid parse of the input.

  when (badParseOfInput parseResult)
       (ioError $ userError "Invalid input data for puzzle 4.")

  let rangePairs = (fst . head) parseResult
      relationshipStatus   = map relationship rangePairs
      fullyContainedCount  = (length . filter oneRangeContained) relationshipStatus
      overlapInAnyWayCount = (length . filter overlapsInAnyWay) relationshipStatus

  return (fullyContainedCount, overlapInAnyWayCount)

-- Returns tre if either of the ranges is contained entirely in the other.

oneRangeContained :: RangeRelationship -> Bool
oneRangeContained rel = rel == Contained1In2 || rel == Contained2In1

-- Returns true if the ranges overlap in any way including one completely contained in the other.

overlapsInAnyWay :: RangeRelationship -> Bool
overlapsInAnyWay rel = rel == Overlap1LowerThan2 || rel == Overlap2LowerThan1
                       || oneRangeContained rel

-- Return the relationship between the two ranges.

relationship :: RangePair -> RangeRelationship
relationship rel@((low1, high1), (low2, high2))
  | low1 > high1 = BadRange1
  | low2 > high2 = BadRange2
  | high1 < low2 = Disjoint1LowerThan2
  | high2 < low1 = Disjoint2LowerThan1
  | low1 >= low2 && high1 <= high2 = Contained1In2
  | low2 >= low1 && high2 <= high1 = Contained2In1
  | high1 >= low2 = Overlap1LowerThan2
  | high2 >= low1 = Overlap2LowerThan1
  | otherwise = error ("Undetermined relationship: " ++ show rel)

-- Parse a list of zero or more range pairs.

readRangePairList :: Parser [RangePair]
readRangePairList = many readRangePair

-- Parse a single range pair.

readRangePair :: Parser RangePair
readRangePair = do
  lowRange <- readRange
  _ <- symbol ","
  highRange <- readRange
  _ <- space
  return (lowRange, highRange)

-- Parse a single range.

readRange :: Parser SingleRange
readRange = do
  low <- int
  _ <- symbol "-"
  high <- int
  return (low, high)
