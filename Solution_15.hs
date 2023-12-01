-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

{-# Language MultiWayIf #-}

module Solution_15 (
  puzzle_15
) where

import Data.List
import Data.Int
import Data.Maybe
import Data.Function
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Parsers

type Location = (Int, Int)
type Dist2D   = (Int, Int)
type BeaconColSet = S.Set Int
type BeaconRowMap = M.Map Int BeaconColSet
data SensorData = SensorData { _loc           :: Location
                             , _beaconLoc     :: Location
                             , _manhattanDist :: Int
                             } deriving Show

--
-- Code for Puzzle 15.
--

puzzle_15 :: IO (Int, Int64)
puzzle_15 = do

  -- Here are the puzzle-specific parameters that are not part of the input file, but are specified
  -- both for the example input and the full input.

  let (inputFile, part1CountRow, part2Multiplier, minXYBound, maxXYBound)
        = ("puzzle_15.inp", 2000000, 4000000, 0, 4000000)
  --  let (inputFile, part1CountRow, part2Multiplier, minXYBound, maxXYBound)
  --        = ("puzzle_15a.inp", 10, 4000000, 0, 20)

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure. Order the sensors with the largest Manhattan distance in the front, which improved
  -- early merging in part 2.

  when (badParseOfInputLines puzzInp)
       (ioError $ userError "Bad input for puzzle 15.")

  let inputData = (sortBy (flip compare `on` _manhattanDist) . map (fst . head)) puzzInp

  -- Create a map keyed off the Y value of each beacon seen where the element is a set with the X
  -- values of each beacon in that row. This is used to subtract the number of beacons in a row from
  -- the known ranges, so as not to count actual beacons as empty spots.
  -- Compute the count for part 1.

  let beaconMap = (foldl' insertBeacon M.empty . map _beaconLoc) inputData
      ansPart1  = countKnownNonBeaconForRow part1CountRow  beaconMap inputData

  -- Create an array filled with the known ranges for each Y value in sorted order. Each new range
  -- added to a list is inserted in sorted order, or merged if possible. Finally, find any range
  -- lists with gaps.

  let gridBounds = (minXYBound, maxXYBound)
      knownArr :: A.Array Int [Dist2D]
      knownArr = A.accumArray insertAndMerge [] gridBounds
                 (concatMap (genXRanges gridBounds) inputData)
      ysWithGap = (filter ((/= [gridBounds]) . snd) . A.assocs) knownArr

  -- Given the list of Ys with gaps, error check, and return the X and Y values of the single place
  -- a beacon could be. Then compute the answer to part 2.
  
  let (part2XCoord, part2YCoord) = findXCoordAndErrorCheckP ysWithGap gridBounds
      ansPart2 = fromIntegral part2XCoord * part2Multiplier + fromIntegral part2YCoord

  return (ansPart1, ansPart2)

-- Check for errors, making sure there is exactly one spot for the beacon, and return both the X and
-- Y coordinates.

findXCoordAndErrorCheckP :: [(Int, [Dist2D])] -> (Int, Int) -> (Int, Int)
findXCoordAndErrorCheckP ysWithGap (minXYBound, maxXYBound)
  | length ysWithGap /= 1 = error "Zero or more than 1 Y-axis with a gap."
  | rangeCount == 0 || rangeCount > 2 = error "Must be a single space gap where the beacon will be."
  | rangeCount == 1 = let [(low, high)] = nonBeaconRanges
                          beaconXCoord = if low == minXYBound then maxXYBound else minXYBound
                      in  if (low == minXYBound && high /= maxXYBound - 1)
                              || (high == maxXYBound && low /= minXYBound + 1)
                          then error "For a beacon at one end there must be just one spot."
                          else (beaconXCoord, yCoord)
  | otherwise = let [(low1, high1), (low2, high2)] = nonBeaconRanges
                in  if | low1 /= minXYBound || high2 /= maxXYBound
                         -> error "Space at one end as well as a gap."
                       | high1 /= low2 - 2 -> error "Gap of more than one space for beacon."
                       | otherwise -> (high1 + 1, yCoord)
  where
    [(yCoord, nonBeaconRanges)] = ysWithGap
    rangeCount = length nonBeaconRanges

-- For a given sensor, return a list of all scanned X ranges paired with the associated Y value.

genXRanges :: (Int, Int) -> SensorData -> [(Int, Dist2D)]
genXRanges (minXYBound, maxXYBound) (SensorData (sensX, sensY) _ manhDist) = yRangePairs
  where
    yRangePairs = [(y, (max minXYBound (minX + yDiff), min maxXYBound (maxX - yDiff)))
        | y <- [lowY..highY], let yDiff = abs (sensY - y)]
    lowY  = max minXYBound (sensY - manhDist)
    highY = min maxXYBound (sensY + manhDist)
    minX  = sensX - manhDist
    maxX  = sensX + manhDist

-- Return the count of know non-beacon spaces for the given row.

countKnownNonBeaconForRow :: Int -> BeaconRowMap -> [SensorData] -> Int
countKnownNonBeaconForRow yValue beaconMap sensorData = count
  where
    count = sensorSpanCount - beaconCountForRow
    sensorSpanCount = (sumRanges . mergeOverlapping . sortBy (compare `on` fst)
--                      . catMaybes . map (getDistFromSensors yValue)) sensorData
                      . mapMaybe (getDistFromSensors yValue)) sensorData
    beaconCountForRow = maybe 0 S.size (M.lookup yValue beaconMap)

-- Insert the x,y coordinates of a beacon.

insertBeacon :: BeaconRowMap -> Location -> BeaconRowMap
insertBeacon accMap (x, y) = M.insertWith S.union y (S.singleton x) accMap

-- Sum the covered ranges. Note that an (i, i) range has length 1.

sumRanges :: [Dist2D] -> Int
sumRanges = foldl' (\acc (low, high) -> acc + (high - low) + 1) 0

-- Works on a list of ranges sorted by low value. Merge overlapping ranges.

mergeOverlapping :: [Dist2D] -> [Dist2D]
mergeOverlapping [] = []
mergeOverlapping [d] = [d]
mergeOverlapping ((low1, high1) : (low2, high2) : ds)
  | low1 > low2 = error "Ranges not sorted by low range value."
  | high1 < low2 - 1 = (low1, high1) : mergeOverlapping ((low2, high2) : ds)
  | otherwise = mergeOverlapping ((low1, max high1 high2) : ds)

-- Given a sorted list of ranges and a a single range, insert/merge it into the list.

insertAndMerge :: [Dist2D] -> Dist2D -> [Dist2D]
insertAndMerge [] range = [range]
insertAndMerge accss@(acc1@(accLow, accHigh) : accs) curr@(currLow, currHigh)
  | currHigh + 1 < accLow = curr : accss
  | currLow - 1 > accHigh = acc1 : insertAndMerge accs curr
  | currLow >= accLow && currHigh <= accHigh = accss
  | otherwise = let newLow   = min accLow currLow
                    newHigh  = newLow `seq` max accHigh currHigh
                    newRange = newHigh `seq` (newLow, newHigh)
                in  newRange `seq` insertAndMerge accs newRange

-- Given a Y-value indicating a row and the data for a sensor, return Nothing if this sensor is too
-- far away from this row to see any part of it, and (Just range) if it is close enough, with the
-- range being the lowest and highest X-value of that range.

getDistFromSensors :: Int -> SensorData -> Maybe Dist2D
getDistFromSensors row (SensorData (sensX, sensY) _ manhattanDist)
  | vertDist > manhattanDist = Nothing
  | otherwise = Just (sensX - distToEachSide, sensX + distToEachSide)
  where
    vertDist = abs (row - sensY)
    distToEachSide = manhattanDist - vertDist

-- Parse an input line.

readInputLine :: Parser SensorData
readInputLine = do
  _ <- symbol "Sensor at x="
  sensX <- int
  _ <- symbol ", y="
  sensY <- int
  _ <- symbol ": closest beacon is at x="
  beacX <- int
  _ <- symbol ", y="
  beacY <- int
  let manhattanDist = abs (sensX - beacX) + abs (sensY - beacY)
  return (SensorData (sensX, sensY) (beacX, beacY) manhattanDist)
