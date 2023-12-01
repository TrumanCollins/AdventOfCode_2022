-- For Advent of Code 2022
--
-- By Truman Collins
-- December 8, 2022

module Solution_08 (
  puzzle_08
) where

import Data.List
import Data.Char
import qualified Data.Set as Set
import qualified Data.Array.Unboxed as UA
import Control.Applicative
import Control.Monad
import Parsers


--
-- Code for Puzzle 8.
--

data TreeDir = Horiz | Vert deriving Eq

puzzle_08 :: IO (Int, Int)
puzzle_08 = do

  -- Read in the file and convert it to a two-dimensional array of heights.

  inputArr <- readArrayOfDigits True "puzzle_08.inp" "8"

  let ((xLow, yLow), (xHigh, yHigh)) = UA.bounds inputArr

      -- Define conditions to work inward from each of the four sides.

      xRange    = [xLow..xHigh]
      xRevRange = [xHigh,xHigh-1..xLow]
      yRange    = [yLow..yHigh]
      yRevRange = [yHigh,yHigh-1..yLow]
      fourDirs  = [(Horiz, xRange, yRange), (Horiz, xRevRange, yRange),
                   (Vert,  xRange, yRevRange), (Vert,  xRange, yRange)]
      visSetsFrom4Sides = map (countVisible inputArr) fourDirs
      allTreesSet = Set.unions visSetsFrom4Sides
      part1Ans = Set.size allTreesSet

      part2Ans = (maximum . map (computeScenicScore inputArr) . UA.indices) inputArr
      
  return (part1Ans, part2Ans)

-- The triple passed in defines the parameters to work from one side to the other for all lines
-- beginning on the four sides of the array.

countVisible :: UA.Array (Int, Int) Int -> (TreeDir, [Int], [Int]) -> Set.Set (Int, Int)
countVisible treeArr (horizOrVert, xRange, yRange) = Set.fromList allCoordsWithVisTrees
  where
    allCoordsWithVisTrees = concatMap (fst . foldl' accumVisible ([], 0)) linesOfCoords
    linesOfCoords
      | horizOrVert == Horiz = [[(x, y) | x <- xRange] | y <- yRange]
      | otherwise = [[(x, y) | y <- yRange] | x <- xRange]

    accumVisible :: ([(Int, Int)], Int) -> (Int, Int) -> ([(Int, Int)], Int)
    accumVisible currAcc@(visList, accHeight) currCoord
      | null visList = ([currCoord], currHeight)
      | currHeight > accHeight = (currCoord : visList, currHeight)
      | otherwise = currAcc
      where
        currHeight = treeArr UA.! currCoord

-- For a specific coordinate among the trees, compute the scenic score for that tree.

computeScenicScore :: UA.Array (Int, Int) Int -> (Int, Int) -> Int
computeScenicScore treeArr coord@(x, y)
  | x < xLow || x > xHigh || y < yLow || y > yHigh = error "Element out of range."
  | otherwise = (product . map countVisTrees) [toWest, toNorth, toEast, toSouth]
  where
    toWest  = [(x1, y) | x1 <- [x-1,x-2..xLow]]
    toEast  = [(x1, y) | x1 <- [x+1..xHigh]]
    toNorth = [(x, y1) | y1 <- [y+1..yHigh]]
    toSouth = [(x, y1) | y1 <- [y-1,y-2..yLow]]
    centerTreeHeight = treeArr UA.! coord
    ((xLow, yLow), (xHigh, yHigh)) = UA.bounds treeArr

    -- Count the visible trees from the origin given a series of retreating coordinates in one of
    -- the four directions. This function doesn't require them to be in a particular direction, but
    -- that the sequence will follow the same rules.
 
    countVisTrees :: [(Int, Int)] -> Int
    countVisTrees xs = beforeRivalHeight + if null rest then 0 else 1
      where
        beforeRivalHeight = length smaller
        (smaller, rest) = span (< centerTreeHeight) heights
        heights = map (treeArr UA.!) xs

-- Read an array of digits from the given file for the given puzzle and return the values in a
-- two-dimensional array of digits. If flipYOrder is set, then the (0, 0) element will be the bottom
-- left textually, whereas if it is false, then the (0, 0) element will be the top left textually.

readArrayOfDigits :: Bool -> String -> String -> IO (UA.Array (Int, Int) Int)
readArrayOfDigits flipYOrder fileName puzzleID = do
  parseRes <- fmap (map (parse parseDigitLine) . lines) (readFile fileName)

  when (badParseOfInputLines parseRes)
       (ioError $ userError ("Parse error in input for puzzle " ++ puzzleID ++ "."))

  let valListsAsRead = map (fst . head) parseRes
      valListsForArr = if flipYOrder then reverse valListsAsRead else valListsAsRead

      -- Here we have the array of heights, and from it compute a list of the low point
      -- coordinates. Both answers will come from this list of low points.

      valArr  = convertDoubleListOfIntsTo2DArray valListsForArr
  return valArr

  where

    -- Parse all of the digits in the string as individual ints.

    parseDigitLine :: Parser [Int]
    parseDigitLine = do
      inString <- many digit
      return (fmap digitToInt inString)

-- Convert a list of int lists to a two dimensional array, with (0, 0) being the first int in the
-- first list, and progressing positively for both X and Y from there. The int lists must all be of
-- the same length.

convertDoubleListOfIntsTo2DArray :: [[Int]] -> UA.Array (Int, Int) Int
convertDoubleListOfIntsTo2DArray xss = UA.array ((0, 0), (colLimit, rowLimit)) arrInitList
  where
    arrInitList = zip [(x, y) | y <- [0..rowLimit], x <- [0..colLimit]] (concat xss)
    rowLimit = length xss - 1
    colLimit = (if null xss then 0 else (length . head) xss) - 1
