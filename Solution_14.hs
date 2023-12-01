-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_14 (
  puzzle_14
) where

import Data.List
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.ST as STA
import Data.STRef
import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Parsers

type RockLoc = (Int, Int)
type RockStructure = [RockLoc]
type CaveArray = UA.UArray RockLoc CaveContent

-- Define the cave contents this way so we can use them as the contents of an unboxed array.

type CaveContent = Int
caveOpen :: CaveContent
caveOpen = 0
caveRock :: CaveContent
caveRock = 1
caveSand :: CaveContent
caveSand = 2

-- The location where sand enters the cave.

initialXSand :: Int
initialXSand = 500
initialYSand :: Int
initialYSand = 0


--
-- Code for Puzzle 14.
--

-- I implemented the function that fills the cave with sand in two different ways, both using the ST
-- monad so as to use mutable arrays. Both of these functions work for both parts of the problem and
-- performance is similar, but the first uses direct recursion, and the second uses a while loop.

puzzle_14 :: IO (Int, Int)
puzzle_14 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile "puzzle_14.inp")

  when (badParseOfInputLines puzzInp)
       (ioError $ userError "Bad input for puzzle 14.")

  -- Create an array holding the initial contents of the cave for part 1. The cave will contain the
  -- rock described in the input and empty spaces everywhere else. We make the bounds one space away
  -- from the rock left and right as well as down. We use the extra empty space with the maximum Y
  -- bound to test when we are done with part 1.

  let inputData = map (fst . head) puzzInp
      (minX, maxX, maxY) = foldl' accMinMax (initialXSand + 1, initialXSand - 1, 0)
                           (concat inputData)
      arrBoundsP1 = ((minX - 1, 0), (maxX + 1, maxY + 1))
      initialRock = concatMap genRocks inputData
      initialCaveArrP1 :: CaveArray
      initialCaveArrP1 = UA.accumArray (\_ x -> x) caveOpen arrBoundsP1 initialRock
      (_, part1Ans) = runST $ fillCaveWithSandW initialCaveArrP1

  -- For part 2, we need to do some work to determine the boundary of the cave. In the Y direction,
  -- we add two rows, the last of which will be filled with rock. In the X direction, we make it
  -- wide enough that all of the rock in the cave fits with at least one more space on each side,
  -- plus, if needed, enough space to build a 45 degree pyramid from the initial sand entry point.

  let maxYP2 = maxY + 2
      minXP2 = min (minX - 1) (initialXSand - maxYP2)
      maxXP2 = max (maxX + 1) (initialXSand + maxYP2)
      arrBoundsP2 = ((minXP2, 0), (maxXP2, maxYP2))
      initialRockP2 = initialRock ++ [((x, maxYP2), caveRock) | x <- [minXP2..maxXP2]]
      initialCaveArrP2 :: CaveArray
      initialCaveArrP2 = UA.accumArray (\_ x -> x) caveOpen arrBoundsP2 initialRockP2
      (_, part2Ans) = runST $ fillCaveWithSand initialCaveArrP2

  return (part1Ans, part2Ans)

-- Fill the cave to the stopping point with sand dripped in from the point at (500, 0). The stopping
-- point is either when a block of sand drops to the lowest level of the cave (part 1) or when the
-- initial entry point is already filled (part 2). This function uses the ST monad to modify the
-- cave array, and the internal single step function is called recursively at each step each sand
-- block takes.

fillCaveWithSand :: CaveArray -> ST s (CaveArray, Int)
fillCaveWithSand caveWithJustRock = do
  thawedCave <- STA.thaw caveWithJustRock :: ST s (STA.STUArray s RockLoc CaveContent)
  (filledCave, sandCount) <- singleStepSandDrop initialSandLoc thawedCave 0
  retArr <- STA.freeze filledCave
  return (retArr, sandCount)

  where

    -- Information used in the functions below.

    initialSandLoc = (initialXSand, initialYSand)
    ((lowX, lowY), (highX, highY)) = UA.bounds caveWithJustRock
    freeDropRow = highY

    -- Take one step for a sand block with a recursive call for the next step. If the block comes to
    -- rest, then recursively call with another block at the entry point and bump up the count.

    singleStepSandDrop :: RockLoc -> STA.STUArray s RockLoc CaveContent -> Int
                          -> ST s (STA.STUArray s RockLoc CaveContent, Int)
    singleStepSandDrop currLoc@(x, y) currArr currCount = do

      -- First make sure the location is in the bounds of the array, and that it is an empty spot.
      -- We also want to make sure that the count value is strict.

      when (currCount `seq` outOfBounds currLoc)
        (error ("Sand location out of bounds: " ++ show currLoc))

      currContents <- STA.readArray currArr currLoc
      
      -- If we have hit the bottom row of the cave or the current contents are sand, which can only
      -- happen at the initial spot when we have completed part 2, we are done and return the count.

      if y == freeDropRow || currContents == caveSand then return (currArr, currCount)
      else do

        let oneBelow = (x, y + 1)
            oneBelowLeft  = (x - 1, y + 1)
            oneBelowRight = (x + 1, y + 1)
            possibleDrops = filter inBounds [oneBelow, oneBelowLeft, oneBelowRight]

        contents <- mapM (\loc -> do
                                    cont <- STA.readArray currArr loc
                                    return (loc, cont)) possibleDrops
        let availDrops = filter ((== caveOpen) . snd) contents

        if null availDrops then do
          STA.writeArray currArr currLoc caveSand
          singleStepSandDrop initialSandLoc currArr (currCount + 1)
        else do
          let newLoc = (fst . head) availDrops
          singleStepSandDrop newLoc currArr currCount

    -- These are used to determine if a location is valid for the cave array.

    inBounds :: RockLoc -> Bool
    inBounds (x, y) = x >= lowX && x <= highX && y >= lowY && y <= highY

    outOfBounds :: RockLoc -> Bool
    outOfBounds = not . inBounds

-- Fill the cave to the stopping point with sand dripped in from the point at (500, 0). The stopping
-- point is either when a block of sand drops to the lowest level of the cave (part 1) or when the
-- initial entry point is already filled (part 2). This function uses the ST monad to modify the
-- cave array, and a while loop is used for each incremental step of a sand block. This gives the
-- same result for both parts of this problem as the other recursive function above, but this uses a
-- monadic while loop, although internally this is recursive as well. The performance of both is
-- nearly identical.

fillCaveWithSandW :: CaveArray -> ST s (CaveArray, Int)
fillCaveWithSandW caveWithJustRock = do

  -- Thaw the cave array so we can modify individual elements without copying it.
  
  thawedCave <- STA.thaw caveWithJustRock :: ST s (STA.STUArray s RockLoc CaveContent)

  -- Create refs for the count and current (x, y) values.

  sandBlockCount <- newSTRef 0
  locR <- newSTRef initialSandLoc

  -- Loop while we haven't reached the very bottom row of the cave and the current location isn't
  -- sand, which can only happen when we drop a new sand block at the very top. Otherwise either
  -- move the sand down one step, or place it and start again at the top.

  whileM_ (do
             loc@(_, y) <- readSTRef locR
             currCont <- STA.readArray thawedCave loc
             return (y < freeDropRow && currCont /= caveSand)
          ) $ do

                -- From the current location, find the places below that the sand might fall to, and
                -- make sure these are in order of priority.

                loc@(x, y) <- readSTRef locR
                let possibleDrops = filter inBounds [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

                -- For each of these locations, associate it will the contents of the cave there.

                contents <- mapM (\lc -> do
                                           cont <- STA.readArray thawedCave lc
                                           return (lc, cont)) possibleDrops

                -- Find all of the locations above that are open spots in the cave.

                let availDrops = filter ((== caveOpen) . snd) contents

                -- If there are no open spots in the cave among the three spaces below the current
                -- location, then put the current sand block at the current location and make the
                -- location of the next sand block to consider be the initial location. Increment
                -- the count of sand blocks placed.
                -- If one of the below spots is open, set the current sand location to that spot,
                -- chosen by priority, and continue.

                if null availDrops then do
                  STA.writeArray thawedCave loc caveSand
                  writeSTRef locR initialSandLoc
                  modifySTRef' sandBlockCount (+ 1)
                else writeSTRef locR ((fst . head) availDrops)

  -- We are done placing sand blocks. Return the current state of the array and the number of sand
  -- blocks that were placed.

  retArr <- STA.freeze thawedCave
  sandCount <- readSTRef sandBlockCount
  return (retArr, sandCount)

  where

    -- Information used in the functions below.

    initialSandLoc = (initialXSand, initialYSand)
    ((lowX, lowY), (highX, highY)) = UA.bounds caveWithJustRock
    freeDropRow = highY

    -- These are used to determine if a location is valid for the cave array.

    inBounds :: RockLoc -> Bool
    inBounds (x, y) = x >= lowX && x <= highX && y >= lowY && y <= highY

-- From the input, generate the list of cave locations and a rock element to go with them.

genRocks :: RockStructure -> [(RockLoc, CaveContent)]
genRocks [(x, y)] = [((x, y), caveRock)]
genRocks rockStructure
  = zip (concatMap genRockLine (zip rockStructure (tail rockStructure))) (repeat caveRock)
  where
    genRockLine ((x1, y1), (x2, y2))
      | x1 == x2 = if y1 <= y2 then [(x1, y) | y <- [y1..y2]] else [(x1, y) | y <- [y2..y1]]
      | y1 == y2 = if x1 <= x2 then [(x, y1) | x <- [x1..x2]] else [(x, y1) | x <- [x2..x1]]
      | otherwise = error "Rock lines must be straight."

-- Accumulate the minimum and maximum X-values and the maximum Y-value.

accMinMax :: (Int, Int, Int) -> (Int, Int) -> (Int, Int, Int)
accMinMax (accMinX, accMaxX, accMaxY) (x, y) = newMinX `seq` (newMinX, newMaxX, newMaxY)
  where
    newMinX = newMaxX `seq` if x < accMinX then x else accMinX
    newMaxX = newMaxY `seq` if x > accMaxX then x else accMaxX
    newMaxY = if y > accMaxY then y else accMaxY

-- Parse an input line.

readInputLine :: Parser RockStructure
readInputLine = do
  rockLine <- readRockLoc
  rockLines <- many readRockLineWithArrow
  return (rockLine : rockLines)

-- Read an arrow and a rock location.

readRockLineWithArrow :: Parser RockLoc
readRockLineWithArrow = do
  _ <- space
  _ <- symbol "->"
  _ <- space
  readRockLoc

-- Read in a single rock location.

readRockLoc :: Parser RockLoc
readRockLoc = do
  n1 <- nat
  _ <- symbol ","
  n2 <- nat
  return (n1, n2)
