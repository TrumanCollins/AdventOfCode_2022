-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_24 (
  puzzle_24
) where

import Data.List
import Data.Maybe
import Data.Either
import Data.Function
import qualified Data.Array as A
import Control.Monad
import Utilities

data Contents = Empt | Wall | Me | NorthBliz | EastBliz | SouthBliz | WestBliz deriving (Eq, Show)

type MapLoc = (Int, Int)
type MapLocHist = [MapLoc]
type PersonPaths = [MapLocHist]
type MapArr = A.Array MapLoc [Contents]

data ValleyStatus = ValleyStatus { _minute      :: Int
                                 , _blizzardMap :: MapArr
                                 , _personLocs  :: PersonPaths
                                 } deriving (Show)

--
-- Code for Puzzle 24.
--

puzzle_24 :: IO (Int, Int)
puzzle_24 = do

  let (inputFile, errorStr) = ("puzzle_24.inp", "Error in puzzle 24: ")

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap lines (readFile inputFile)

  let mapOrigin = (0, 0)
      mapArrOrError = construct2DArrayFromInputStrings LowerLeft mapOrigin convFn puzzInp

  when (isLeft mapArrOrError)
    (let errorCode = fromLeft UnknownError mapArrOrError
         errorMessage = correspondingArrCreationErrorMessage errorCode
     in  ioError $ userError (errorStr ++ errorMessage))
  
  let mapArr = fromRight undefined mapArrOrError
      ((_, yLow), (_, yHigh)) = A.bounds mapArr

  -- Find the top and bottom empty locations in the wall.

  let topLocM = findEmptyLocationInRow yHigh mapArr
      botLocM = findEmptyLocationInRow yLow mapArr

  when (isNothing topLocM || isNothing botLocM)
    (ioError $ userError (errorStr ++ "No start or end location in wall."))

  -- Generate the answer for part 1, and it is also the beginning of the solutions for part 2.

  let topLoc = fromJust topLocM
      botLoc = fromJust botLocM
      initialStatePart1 = ValleyStatus 0 mapArr [[topLoc]]
      (finalStateP1, pathToGoal1) = iterateToGoal initialStatePart1 botLoc
      ansPart1 = _minute finalStateP1

  let headingBack = finalStateP1 {_personLocs = [pathToGoal1]}
      (stateBackToStart, pathToStart) = iterateToGoal headingBack topLoc
      headingToEnd = stateBackToStart {_personLocs = [pathToStart]}
      (stateAtEnd, _) = iterateToGoal headingToEnd botLoc
      ansPart2 = _minute stateAtEnd
  
  return (ansPart1, ansPart2)

-- Iterate through a depth-first search of making one's way through the blizzard valley until
-- reaching the goal location. Return the status of the valley and various possible paths along with
-- the single path that got you to the goal..

iterateToGoal :: ValleyStatus -> MapLoc -> (ValleyStatus, MapLocHist)
iterateToGoal valleyStatus goalLoc
  | isNothing pathToGoalM = error "Puzzle 24: No path found to goal."
  | otherwise = (finalValleyStatus, fromJust pathToGoalM)
  where
    pathToGoalM = find ((== goalLoc) . head) (_personLocs finalValleyStatus)
    finalValleyStatus = (head . dropWhile (notAtGoal goalLoc) . iterate nextState) valleyStatus
    
    -- Generate the next state of the valley after one minute has passed. This includes updating the
    -- blizzards and the possible locations for a person.

    nextState :: ValleyStatus -> ValleyStatus
    nextState (ValleyStatus minute blizzardMap personPaths) = nextValleyState
      where
        nextValleyState = nextMinute `seq` ValleyStatus nextMinute nextBlizzardMap nextPersonPaths
        nextMinute = minute + 1
        nextBlizzardMap = updateBlizzardMap blizzardMap
        nextPersonPaths = updatePersonPaths nextBlizzardMap personPaths

    -- Returns true if no possible locations at the point has the person in the goal location.

    notAtGoal :: MapLoc -> ValleyStatus -> Bool
    notAtGoal goalLoc' (ValleyStatus _ _ personPaths)
      | null goodLocs = True
      | otherwise = False
      where
        goodLocs = filter ((== goalLoc') . head) personPaths

-- Advance the blizzard map by one minute.

updateBlizzardMap :: MapArr -> MapArr
updateBlizzardMap mapArr = A.accumArray (flip (:)) [] (A.bounds mapArr) initList
  where
    initList = (foldr handleOneLoc [] . A.assocs) mapArr
    ((xLow, yLow), (xHigh, yHigh)) = A.bounds mapArr

    -- Find the next location for the contents of this location. For walls, leave them where they
    -- are, and for empty spaces, there is no need to do anything, so ignore them.

    handleOneLoc :: (MapLoc, [Contents]) -> [(MapLoc, Contents)] -> [(MapLoc, Contents)]
    handleOneLoc (mapLoc@(x, y), contents) acc
      | null contents = acc
      | contents == [Wall] = (mapLoc, Wall) : acc
      | otherwise = foldr handleOneBlizzard acc contents
      where

        -- Find the next location for the current blizzard, which may be to wrap around the map.

        handleOneBlizzard :: Contents -> [(MapLoc, Contents)] -> [(MapLoc, Contents)]
        handleOneBlizzard blizDir accum = (newLoc, blizDir) : accum
          where
            newLoc
              | blizDir == NorthBliz = let newY = y + 1
                                       in  if newY == yHigh then (x, yLow + 1) else (x, newY)
              | blizDir == EastBliz  = let newX = x + 1
                                       in  if newX == xHigh then (xLow + 1, y) else (newX, y)
              | blizDir == SouthBliz = let newY = y - 1
                                       in  if newY == yLow then (x, yHigh - 1) else (x, newY)
              | blizDir == WestBliz  = let newX = x - 1
                                       in  if newX == xLow then (xHigh - 1, y) else (newX, y)
              | otherwise = error ("Invalid content: " ++ show blizDir)
                     

-- Take the list of current person locations and a map from one minute later, and generate an
-- updated person location list. Some people locations may disappear because there is nowhere to go,
-- and some may generate more than one new location.

updatePersonPaths :: MapArr -> PersonPaths -> PersonPaths
updatePersonPaths mapArr
  = map head . groupBy ((==) `on` head) . sortBy (compare `on` head) . concatMap nextMoves
  where
    ((xLow, yLow), (xHigh, yHigh)) = A.bounds mapArr
    
    nextMoves :: MapLocHist -> [MapLocHist]
    nextMoves [] = error "The function nextMoves should never be called with an empty move list."
    nextMoves hist@((x, y) : _) = map (: hist) availableMoves
      where
        availableMoves = (filter isEmpty . filter inBounds) allPossibleMoves
        allPossibleMoves = [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

        isEmpty :: MapLoc -> Bool
        isEmpty loc = null (mapArr A.! loc)

        inBounds :: MapLoc -> Bool
        inBounds (x', y')
          | x' < xLow || x' > xHigh = False
          | y' < yLow || y' > yHigh = False
          | otherwise = True

-- Search the given row for the first empty location on the map and return that location in a
-- Maybe. Nothing if no empty location.

findEmptyLocationInRow :: Int -> MapArr -> Maybe MapLoc
findEmptyLocationInRow y mapArr = maybeLoc
  where
    maybeLoc = foldr findEmptyLoc Nothing [xLow..xHigh]
    ((xLow, _), (xHigh, _)) = A.bounds mapArr

    findEmptyLoc :: Int -> Maybe MapLoc -> Maybe MapLoc
    findEmptyLoc x acc
      | null (mapArr A.! currLoc) = Just currLoc
      | otherwise = acc
      where
        currLoc = (x, y)

-- Convert an input character to a one-element content list. Used for array construction.

convFn :: Char -> Maybe [Contents]
convFn ch
  | ch == '.' = Just []
  | ch == '#' = Just [Wall]
  | ch == '^' = Just [NorthBliz]
  | ch == '>' = Just [EastBliz]
  | ch == 'v' = Just [SouthBliz]
  | ch == '<' = Just [WestBliz]
  | otherwise = Nothing
