-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_12 (
  puzzle_12
) where

import Data.Maybe
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.ST as STA
import Control.Monad
import Control.Monad.ST


type Location = (Int, Int)
type PathAndLen = ([Location], Int)
type ElevArray = UA.Array Location Char

--
-- Code for Puzzle 12.
--

-- Both parts of this problem can be solved using a graph theory package like fgl. I decided to
-- write my own depth-first and breadth-first searches using the ST monad to update an array with
-- data about the search. In hindsight, part 1 takes longer than it should, probably because there
-- are so many paths that come back on each other that a breadth-first search would be much faster.

puzzle_12 :: IO (Int, Int)
puzzle_12 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap lines (readFile "puzzle_12.inp")

  let rows = length puzzInp
      rowLens = map length puzzInp
      firstRowLen = head rowLens

  -- Make sure the size of the data isn't zero, and make sure that all of the row lenghts are the
  -- same. Note that firstRowLen won't be used unless we know there is more than zero rows.

  when (rows == 0 || firstRowLen == 0 || any (/= firstRowLen) (tail rowLens))
    (ioError $ userError "Non-uniform or empty input data for 12.")

  -- Create an elevation array from the input data indexed by a tuple with origin at the lower left
  -- of the data as seen in the input file. Also, record where the start and end locations are.

  let xMax = firstRowLen - 1
      yMax = rows - 1
      withIndices :: [(Location, Char)]
      withIndices = zip [(x, y) | y <- [yMax, yMax - 1..0], x <- [0..xMax]] (concat puzzInp)
      (start, end, initList) = foldr accStartEndInit ((-1, -1), (-1, -1), []) withIndices
      elevArr :: ElevArray
      elevArr = UA.array ((0, 0), (xMax, yMax)) initList

  -- Use the ST monad to efficiently find the shortest path from the start to the end using a
  -- depth-first search. The array used by the ST monad keeps track of the shortest path to each
  -- location, which is used to insure that we don't duplicate paths when not needed.

  let (_, pathLenPart1) = runST $ findShortestPathStartToEnd elevArr start end

  -- For part 2, again use the ST monad to efficiently find the shortest path from the end point to
  -- a point of lowest altitude. We use a breadth-first search this time and an array indicating
  -- visited indices.

  let pathLenPart2M = runST $ findShortestPathToLowestAlt elevArr end 'a'

  when (isNothing pathLenPart2M)
    (ioError $ userError "No path to lowest elevation for puzzle 12.")

  let pathLenPart2 = fromJust pathLenPart2M

  return (pathLenPart1, pathLenPart2)

-- Locate the start and end indices, and replace those characters with 'a' and 'z' respectively.

accStartEndInit :: ((Int, Int), Char) -> ((Int, Int), (Int, Int), [((Int, Int), Char)])
                   -> ((Int, Int), (Int, Int), [((Int, Int), Char)])
accStartEndInit curr@(currInd, currChar) (accStart, accEnd, accList)
  | currChar == 'S' = (currInd, accEnd, (currInd, 'a') : accList)
  | currChar == 'E' = (accStart, currInd, (currInd, 'z') : accList)
  | otherwise = (accStart, accEnd, curr : accList)

-- Use the ST monad to keep an array of steps to each element. Do a depth-first search to
-- find the sortest path.

findShortestPathStartToEnd :: ElevArray -> Location -> Location -> ST s ([Location], Int)
findShortestPathStartToEnd elevArr start end
  = do
      stepsToArr <- STA.newArray elevArrBounds maxBound :: ST s (STA.STUArray s Location Int)
      ((shortestPath, len), _) <- dfsShortestPath ([], -1) (([], maxBound), stepsToArr) start
      return (shortestPath, len)
  where
    elevArrBounds@((lowX, lowY), (highX, highY)) = UA.bounds elevArr

    dfsShortestPath :: PathAndLen -> (PathAndLen, STA.STUArray s Location Int)
                       -> Location -> ST s (PathAndLen, STA.STUArray s Location Int)
    dfsShortestPath (currPath, currLen) acc@((_, accBestLen), accLenArr) nextLoc@(x, y)

      -- If adding this new step results in a path equal to or longer than the best path we have
      -- found so far, then there is no need to explore this path further.

      | nextLen >= accBestLen = return acc

      -- If the next step gets us to the end goal, then this is the shortest path found so far, so
      -- return the current path as the best.

      | nextLoc == end = return ((nextLoc : currPath, nextLen), accLenArr)

      | otherwise = do

          -- If the next location has already been visited by a prior depth-first search branch, and
          -- the number of steps to it on that branch was the same or smaller, then there is no need
          -- to explore further.

          nextLocShortestPath <- STA.readArray accLenArr nextLoc
          if nextLocShortestPath <= nextLen then return acc
          else do

            -- Find the next-door locations that are legal to move to, and do a depth first search
            -- on each of them, generating a new best-path accumulator.

            let nextPath = nextLoc : currPath
                neighborLocs = filter legalMove [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
            STA.writeArray accLenArr nextLoc nextLen
            foldM (dfsShortestPath (nextPath, nextLen)) acc neighborLocs
      where
        nextLen  = currLen + 1
        nextMaxElev = succ (elevArr UA.! nextLoc)

        -- This is a legal move if the index is in range, and the elevation is one higher or less
        -- than the current elevation.

        legalMove :: (Int, Int) -> Bool
        legalMove loc@(xInd, yInd)
          | xInd < lowX || xInd > highX = False
          | yInd < lowY || yInd > highY = False
          | otherwise = (elevArr UA.! loc) <= nextMaxElev

-- Do a breadth-first search from the end location searching for the first time we reach a lowest
-- elevation, then return the number of steps taken.

findShortestPathToLowestAlt :: ElevArray -> Location -> Char -> ST s (Maybe Int)
findShortestPathToLowestAlt elevArr start destElev
  = do

      -- Create an array indicating which locations have been visited, and set the initial location
      -- as visited. Then begin the breadth-first search from there.

      visitedArr <- STA.newArray elevArrBounds False :: ST s (STA.STUArray s Location Bool)
      STA.writeArray visitedArr start True
      breadthSearch visitedArr [start] 0
  where
    elevArrBounds@((lowX, lowY), (highX, highY)) = UA.bounds elevArr

    -- Given the array of visited locations, a list of current locations, and the current depth,
    -- search one more step from all of these locations.

    breadthSearch :: STA.STUArray s Location Bool -> [Location] -> Int -> ST s (Maybe Int)

    -- If there are no current locations, then we have visited all of the locations possible from
    -- the initial one, and never reached the desired elevation, so return Nothing.

    breadthSearch _ [] _ = return Nothing
    breadthSearch visitedArr currLocs currDepth

      -- If any of the current locations are at the depth we are searching for, then return this
      -- depth.

      | any (\loc -> elevArr UA.! loc == destElev) currLocs = return (Just currDepth)

      -- Here, we want to generate all of the legal moves from each of the current locations. As we
      -- generate them, check to make sure that we haven't visited them before, and then set the
      -- visited flag for it, so if we run across the same new location later while generating the
      -- next steps, we don't include it again.

      | otherwise
        = do
          let nextDepth = currDepth + 1
              nextLocsAll = concatMap genLegalNeighbors currLocs
          nextLocsNotVisited <- filterM (checkVisitedAndSet visitedArr) nextLocsAll
          breadthSearch visitedArr nextLocsNotVisited nextDepth
      where

        -- Generate all of the neighbor locations that are legal (in the bounds of the array and
        -- that have an acceptable elevation for a backward traversal).
 
        genLegalNeighbors :: Location -> [Location]
        genLegalNeighbors currLoc@(x, y)
          = filter legalMove [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
          where

            -- This is a legal move if the index is in range, and the elevation is one lower or more
            -- than the current elevation.

            legalMove :: (Int, Int) -> Bool
            legalMove loc@(xInd, yInd)
              | xInd < lowX || xInd > highX = False
              | yInd < lowY || yInd > highY = False
              | otherwise = (elevArr UA.! loc) >= allowableStepElevOrBigger

            allowableStepElevOrBigger = pred (elevArr UA.! currLoc)

        -- Check whether this location has been visited, and return that. Also, if it has not,
        -- then set the visited flag for this location.

        checkVisitedAndSet :: STA.STUArray s Location Bool -> Location -> ST s Bool
        checkVisitedAndSet visArr loc
          = do
            alreadyVisited <- STA.readArray visArr loc
            if alreadyVisited then return False
            else do
                 STA.writeArray visArr loc True
                 return True
