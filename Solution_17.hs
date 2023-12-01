-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_17 (
  puzzle_17
) where

import Data.Foldable (toList)
import Data.Int
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as SQ

-- Lots of types to define for this one.

data MoveKind = Blow | Drop deriving (Eq, Show)

data MoveDir = LeftDir | RightDir deriving (Eq, Show)

data Content = Rock | Empt deriving (Eq)
instance Show Content where
  show Rock = "*"
  show Empt = "."

type ContentRow = [Content]

type DirID           = Int
type Altitude        = Int64
type Height          = Int64
type RockID          = Int
type RockRow         = SQ.Seq Content
type Rock            = [RockRow]
type IDMoveDir       = (DirID, MoveDir)
type IDMoveDirs      = [IDMoveDir]
type IDHeightRock    = (RockID, Height, Rock)
type FallingRock     = (Altitude, IDHeightRock)
type WaitingRocks    = [IDHeightRock]
type Altitudes       = [Altitude]
type GeoIDState      = (RockID, DirID, Altitudes)
type TallState       = (Int64, Altitude)
type GeoIDMap        = M.Map GeoIDState TallState

type StackedRocks    = [ContentRow]

type SettledState    = (Altitude, StackedRocks)

chamberWidth :: Int
chamberWidth = 7

-- Define the falling shapes as a triple, with an ID number, its height, and the shape defined
-- initially as a list of lists, but converted to a slist of Sequences.

fallingShapes :: WaitingRocks
fallingShapes = map (\(c, h, r) -> (c, h, map SQ.fromList r))
                 [(0, 1, [[Empt, Empt, Rock, Rock, Rock, Rock, Empt]]),

                  (1, 3, [[Empt, Empt, Empt, Rock, Empt, Empt, Empt],
                          [Empt, Empt, Rock, Rock, Rock, Empt, Empt],
                          [Empt, Empt, Empt, Rock, Empt, Empt, Empt]]),

                  (2, 3, [[Empt, Empt, Empt, Empt, Rock, Empt, Empt],
                          [Empt, Empt, Empt, Empt, Rock, Empt, Empt],
                          [Empt, Empt, Rock, Rock, Rock, Empt, Empt]]),

                  (3, 4, [[Empt, Empt, Rock, Empt, Empt, Empt, Empt],
                          [Empt, Empt, Rock, Empt, Empt, Empt, Empt],
                          [Empt, Empt, Rock, Empt, Empt, Empt, Empt],
                          [Empt, Empt, Rock, Empt, Empt, Empt, Empt]]),

                  (4, 2, [[Empt, Empt, Rock, Rock, Empt, Empt, Empt],
                          [Empt, Empt, Rock, Rock, Empt, Empt, Empt]])]

--
-- Code for Puzzle 17.
--

puzzle_17 :: IO (Int64, Int64)
puzzle_17 = do

  let (inputFile, rocksToFallPart1, rocksToFallPart2) = ("puzzle_17.inp", 2022, 1000000000000)

  -- Read in the file and convert it to a list of strings, one per line in the file.

  directionList <- fmap (zip [0..] . readLeftRight) (readFile inputFile)

  -- The first one is straightforward and can be solved by correctly placing all the rocks.

  let finalHeightPart1 = settleRocksToCount rocksToFallPart1 fallingShapes directionList

  -- To solve part 2, keep track of the state of the top of the stack, in terms of depth, along with
  -- the IDs of the next rock and next wind direction. If we run across a duplicate signature, we
  -- know this pattern will repeat, and we can compute the altitude of the stack at the nearest
  -- multiple of this cycle, then just finish off the rest one rock at a time.

  let finalHeightPart2 = settleRocksToCount rocksToFallPart2 fallingShapes directionList

  return (finalHeightPart1, finalHeightPart2)

-- Drop the given number of rocks into an empty canyon. Passed in is the finite sequence of rocks in
-- the queue and the finite sequence of wind directions.

settleRocksToCount :: Int64 -> WaitingRocks -> IDMoveDirs -> Altitude
settleRocksToCount rocksToFall rocksFallingSeries windDirectionSeries
  = fst $ settleRocks Blow 0 emptyChamber signatureMapWithInitial firstRockWithAlt
                      queuedRocks queuedBlowDirs
  where
    emptyChamber = (0, [])
    signatureMapWithInitial = M.singleton (0, 0, [0, 0, 0, 0, 0, 0, 0]) (0, 0)
    firstRockWithAlt = (4, firstRock)
    (firstRock : queuedRocks) = cycle rocksFallingSeries
    queuedBlowDirs = cycle windDirectionSeries

    -- Recursive function that will go through the sequence of dropping and blowing rocks until the
    -- requested number have settled, then return the altitude and state of the column of rocks,
    -- minus replicated chunks that are counted in the altitude but not layed out in detail.

    settleRocks :: MoveKind -> Int64 -> SettledState -> GeoIDMap -> FallingRock -> WaitingRocks
                   -> IDMoveDirs -> SettledState

    settleRocks _ _ _ _ _ _ [] = error "Empty blow list in settleRocks."
    settleRocks Blow rockCount settled geoIDMap currRock@(rockAlt, (rockID, height, rock))
                waitingRocks ((_, mv) : mvs)

      -- If we have let settle all of the rocks requested, then we are done, and return the altitude
      -- of the stack of rocks along with the stack, which may not be of the full height due to
      -- repeated pieces not placed actually placed and only counted.

      | rockCount == rocksToFall = settled

      -- After reconfiguring the dropping rock after being blown left or right by the wind, do a
      -- recursive call to drop it one space.

      | otherwise = settleRocks Drop rockCount settled geoIDMap newRock waitingRocks mvs
      where
        newRock = if rockBlocked || collidedWithRock then currRock else pushedRockState
        pushedRockState  = (rockAlt, (rockID, height, pushedRock))
        collidedWithRock = overlappingRocks pushedRockState settled
        (rockBlocked, pushedRock) = pushRock rock mv

    -- The settle function for a drop of the current rock.

    settleRocks Drop rockCount settled geoIDMap currRock@(rockAlt, (rockID, height, rock))
                waitingRocks mvs@((mvID, _) : _)

      -- If we are either at the bottom of the canyon or trying to drop the rock results in a
      -- collision, then incorporate the rock at this place in the settled rocks and get the next
      -- rock in the queue ready to blow left or right in the next call.

      | rockAlt == 1 || collidedWithRock
        = let (newCurrRock : newWaitingRocks) = waitingRocks
              newSettledAlt = fst newSettled
              nextRockID = (\(x, _, _) -> x) newCurrRock
              (jumpedRockCount, newSettledJumpAlt, newGeoIDMap)
                = jumpIfDupSig newRockCount newSettledAlt nextRockID
              newRockAndAlt = (newSettledJumpAlt + 4, newCurrRock)
              newSettledJumped = (newSettledJumpAlt, snd newSettled)
          in  settleRocks Blow jumpedRockCount newSettledJumped newGeoIDMap newRockAndAlt
                          newWaitingRocks mvs

      -- Here we were able to drop the rock by one space without a collision or it hitting the
      -- bottom, so continue on to the next blow step for this rock.

      | otherwise = settleRocks Blow rockCount settled geoIDMap droppedRockState waitingRocks mvs
      where 
        collidedWithRock = overlappingRocks droppedRockState settled
        droppedRockState = (rockAlt - 1, (rockID, height, rock))
        newSettled = lockRockInPlace settled currRock
        newRockCount = rockCount + 1

        -- We keep a map of signatures of settled states keyed off of the rock ID, the blow
        -- direction ID, and the current 7 rock depths. We generate this signature for the current
        -- state and look it up in the map. If we don't find it, insert it and return the current
        -- rock count, total altitude, and map. If we do find it, then we know we have a repeating
        -- pattern in the settled rock stack, so figure out the rock count and total altitude to get
        -- as close as we can to the rock count we want to reach, and return those values along with
        -- an empty map, since this jumping technique is only really useful once.

        jumpIfDupSig :: Altitude -> Int64 -> RockID -> (Int64, Altitude, GeoIDMap)
        jumpIfDupSig currRockCount currAlt nextRockID
          | isNothing foundSigM = let newGeoIDMap = M.insert altitudeSig toInsert geoIDMap
                                  in  newGeoIDMap `seq` (currRockCount, currAlt, newGeoIDMap)
          | otherwise = let (mapCnt, mapAlt) = fromJust foundSigM
                            cntDiff = currRockCount - mapCnt
                            altDiff = currAlt - mapAlt
                            rocksToGo = rocksToFall - currRockCount
                            loops = rocksToGo `quot` cntDiff
                            jumpedRockCount = currRockCount + loops * cntDiff
                            jumpedAlt = jumpedRockCount `seq` currAlt + loops * altDiff
                        in  jumpedAlt `seq` (jumpedRockCount, jumpedAlt, M.empty)
          where
            altitudeSig = (nextRockID, mvID, (genAltSig . snd) newSettled)
            foundSigM   = M.lookup altitudeSig geoIDMap
            toInsert    = (newRockCount, fst newSettled)

-- This function generates an altitude signature for the stacked rocks. Since the stack is 7 wide,
-- the signature will be a list of 7 ints where each represents how far to look down from the top to
-- find a rock or the base. Move down the stack until we have 7 signature depths.

genAltSig :: StackedRocks -> Altitudes
genAltSig = go 0 0 (replicate chamberWidth (-1))
  where

    -- If we have reached the bottom of the stack, that's as deep as we go, so use this depth for
    -- all signatures not yet filled in.

    go depth _ currAltitudes [] = map (\x -> if x == -1 then depth else x) currAltitudes

    go depth coverCount currAltitudes (xs : xss)

      -- If we have covered all of the 7 slots with values, then we are done.

      | coverCount == chamberWidth = currAltitudes

      -- Here, we set the depth on any rocks in locations with no higher rock, then go on to the
      -- next level.

      | otherwise = let newDepth = depth + 1
                        (addedLevels, newAltitudes) = noteNewRocks currAltitudes xs
                        newCoverCount = newDepth `seq` coverCount + addedLevels
                    in  newCoverCount `seq` go newDepth newCoverCount newAltitudes xss
      where

        -- Fold the current content row into the altitudes we have accumulated so far, and update
        -- the cover count for any new depths added. Note that this function does not use tail
        -- recursion, but the maximum depth will be 7..

        noteNewRocks :: Altitudes -> ContentRow -> (Int, Altitudes)

        -- This is the base case.

        noteNewRocks [] [] = (0, [])

        -- These two cases should never happen, since the lists should be the same length.

        noteNewRocks [] _ = error "Lists not same length."
        noteNewRocks _ [] = error "Lists not same length."

        noteNewRocks (currAlt : restAlts) (currCont : restCont)

          -- If we already have an altitude for this one, move on to the next.

          | currAlt /= -1 = (deeperCount, currAlt : deeperAlts)

          -- If we see a rock, then this is the depth.

          | currCont == Rock = (deeperCount + 1, depth : deeperAlts)

          -- If this is empty, just move on. We will look for a rock in this location at a deeper
          -- level.

          | otherwise = (deeperCount, currAlt : deeperAlts)
          where
            (deeperCount, deeperAlts) = noteNewRocks restAlts restCont

-- Place the given rock into the settled rock structure, and return that new structure along with
-- its new altitude.

lockRockInPlace :: SettledState -> FallingRock -> SettledState
lockRockInPlace (topAlt, settledRocks) (rockAlt, (_, height, rock))

  -- Here the bottom row of the rock block is one higher than the settled rocks, so just add the
  -- rock leyers to the list and we are done. The rock block being added can only be one above the
  -- settled blocks or there is something wrong with the placement logic.

  | rockAlt > topAlt = (rockTopAlt, rockAsLists ++ settledRocks)

  -- Here, the top of the rock block is at or below the to of the settled rocks. We need to
  -- temporarily remove any settled rock layers below the top of the rock block, add in the new rock
  -- block rocks to the settled lists, then add back the settled layers above.

  | rockTopAlt <= topAlt = let layersToLop = fromIntegral (topAlt - rockTopAlt)
                               (upperSettled, lowerSettled) = splitAt layersToLop settledRocks
                               settledWithRockMerged = mergeRockLists rockAsLists lowerSettled
                           in  (topAlt, upperSettled ++ settledWithRockMerged)

  -- Here the rock block to insert overlaps the top of the settled rocks.

  | otherwise = let layersToLop = fromIntegral (rockTopAlt - topAlt)
                    (upperRock, lowerRock) = splitAt layersToLop rockAsLists
                    settledWithRockMerged = mergeRockLists lowerRock settledRocks
                in  (rockTopAlt, upperRock ++ settledWithRockMerged)
  where

    -- Compute the new settled altitude of the settled stack of rocks and convert the rock we are
    -- settling in the stack from a list of sequences to a list of lists.

    rockTopAlt  = rockAlt + height - 1
    rockAsLists = map toList rock

    -- Merge two layer lists where the result lists have a rock if their was a rock in one or both
    -- of the lists to merge.

    mergeRockLists :: [ContentRow] -> [ContentRow] -> [ContentRow]
    mergeRockLists [] xs = xs
    mergeRockLists xs [] = xs
    mergeRockLists (x : xs) (y : ys)
      = zipWith (\w v -> if w == Rock || v == Rock then Rock else Empt) x y : mergeRockLists xs ys

-- Return true of the falling rock is in a place where pieces of the block that are rocks are at the
-- same place as those in the settled rock structure.

overlappingRocks :: FallingRock -> SettledState -> Bool
overlappingRocks (rockAlt, (_, height, rock)) (topAlt, settledRocks)
  | rockAlt > topAlt = False
  | otherwise = or $ zipWith doubleRocks (drop dropFromRock rock) (drop dropFromSettled settledRocks)
  where
    dropFromRock = fromIntegral (max 0 (height - (topAlt - rockAlt + 1)))
    dropFromSettled = fromIntegral (topAlt - (rockAlt + height - 1))

    doubleRocks :: RockRow -> ContentRow -> Bool
    doubleRocks rockRow contentRow = or $ zipWith (\x y -> x == Rock && y == x) (toList rockRow) contentRow

-- Try to shift the rock left or right depending on the gas direction, and return the new block and
-- a boolean indicating if it was shifted or not.

pushRock :: Rock -> MoveDir -> (Bool, Rock)
pushRock rock LeftDir
  | SQ.EmptyL `elem` leftViews = error "Empty rock block."
  | Rock `elem` leftElements = (True, rock)
  | otherwise = (False, map (\(_ SQ.:< xs) -> xs SQ.|> Empt) leftViews)
  where
    leftElements = map (\(x SQ.:< _) -> x) leftViews
    leftViews = map SQ.viewl rock

pushRock rock RightDir
  | SQ.EmptyR `elem` rightViews = error "Empty rock block."
  | Rock `elem` rightElements = (True, rock)
  | otherwise = (False, map (\(xs SQ.:> _) -> Empt SQ.<| xs) rightViews)
  where
    rightElements = map (\(_ SQ.:> x) -> x) rightViews
    rightViews = map SQ.viewr rock

-- Read a left or right symbol and convert to the enumeration.

readLeftRight :: String -> [MoveDir]
readLeftRight = map convToLeftRight
  where
    convToLeftRight '<' = LeftDir
    convToLeftRight '>' = RightDir
    convToLeftRight ch = error ("Unrecognized token: " ++ show ch ++ " in puzzle 17 input.")
