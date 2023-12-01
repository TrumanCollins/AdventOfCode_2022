-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

{-# Language TupleSections #-}

module Solution_23 (
  puzzle_23
) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Bits
import qualified Data.Array.Unboxed as UA
import Control.Monad

data GroundContent = Empt | Elf deriving (Show, Eq)

type GroveLoc = (Int, Int)
type GroveArray = UA.Array GroveLoc GroundContent
type DirInfo = (Int, (Int -> Int, Int -> Int))

--
-- Code for Puzzle 23.
--

puzzle_23 :: IO (Int, Int)
puzzle_23 = do

  let (roundsP1, inputFile) = (10, "puzzle_23.inp")

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap lines (readFile inputFile)

  when (null puzzInp)
       (ioError $ userError "Error: Empty input for puzzle 23.")

  -- Make sure the lengths of the input lines are all the same.

  let (len1 : rest) = map length puzzInp
      allSameLen = all (== len1) rest
  
  unless allSameLen
         (ioError $ userError "Error: Input lines not all the same length for puzzle 23.")
  
  let groveArr = convertInputLinesToArray len1 puzzInp
  let finalGroveP1 = (fst . iterateRounds (True, roundsP1)) groveArr
      ansPart1 = (length . filter ((== Empt) . snd) . UA.assocs) finalGroveP1

  let ansPart2 = (snd . iterateRounds (False, 0)) groveArr

  return (ansPart1, ansPart2)

-- This function takes in a grove array and will iterate moves under two different scenarios,
-- indicated by a boolean (fixedIterCount). If this value is True a fixed number of iterations are
-- completed and the resulting grove array returned along with the number of iterations. When
-- fixedIterCount is False it will iterate until no move elfs move, then return the resulting grove
-- array along with the number of the round where no elf moved.

iterateRounds :: (Bool, Int) -> GroveArray -> (GroveArray, Int)
iterateRounds (fixedIterCount, itersToDo) groveArrTop
  = go 0 dirOrderTop (getElfLocsFromArr groveArrTop) groveArrTop
  where

    -- This infinite list holds the four search directions along with the x and y functions to use
    -- to get the destination location, and these entries are cycled over and over, so the first
    -- four are what we need at the start of each iteration.

    dirOrderTop = cycle [(northClearBits, (id, (+ 1))), (southClearBits, (id, subtract 1)),
                         (westClearBits, (subtract 1, id)), (eastClearBits, ((+ 1), id))]

    -- Do the actual iteration until one of the exit conditions occurs.

    go :: Int -> [DirInfo] -> [GroveLoc] -> GroveArray -> (GroveArray, Int)
    go currIter dirOrder elfLocs groveArr
      | fixedIterCount && currIter == itersToDo = (groveArr, currIter)
      | fixedIterCount = go nextIter nextDirOrder nextElfLocs nextGroveArr
      | not elvesMoved = (nextGroveArr, nextIter)
      | otherwise = go nextIter nextDirOrder nextElfLocs nextGroveArr
      where

        -- Various value used for the next iteration.

        nextIter = currIter + 1
        nextDirOrder = drop 1 dirOrder
        nextGroveArr = UA.accumArray (const id) Empt nextGroveBounds (map (, Elf) nextElfLocs)
        nextGroveBounds = findElfBounds nextElfLocs
        nextElfLocs = mightHaveMovedLocs ++ noMoveLocs

        dirOrder4  = take 4 dirOrder
        ((gaXMin, gaYMin), (gaXMax, gaYMax)) = UA.bounds groveArr
        mightHaveMovedLocs = map fst possLocsMoveOrNot

        -- Set to true if any elves moved.

        elvesMoved = any snd possLocsMoveOrNot

        -- Here we have all of the elves that decided to try to move. It is a list of pairs
        -- containing a location (either its original if it was blocked or new if not, and a boolean
        -- value indicating if it moved or not. Since all of the second entries in possMovers are
        -- Just value, it would seem best to map a fromJust before sorting to remove a level of
        -- hierarchy, but the memory swaps needed seem to take more total time.

        possLocsMoveOrNot = (concatMap handlePossMovers . groupBy ((==) `on` snd)
                             . sortBy (compare `on` snd)) possMovers

        -- These are the elves that didn't even try to move because there we no other elves next to
        -- them or because the four directions to move were blocked.

        noMoveLocs = map fst noMovers

        -- Split the elves into those that won't be moving and those that may move

        (noMovers, possMovers) = partition (isNothing . snd) elvesAndPossNewLocs

        -- Here we have a list of pairs. The first element is the location of an elf in the incoming
        -- grove array. The second is a Maybe location. If it is Nothing, then the elf at that
        -- location either didn't have to move or wasn't able to move from the elves around it in
        -- the incoming grove array. If there is a Maybe location as the second elemnt of the pair,
        -- that is the location the elf can move to. Later we will determine if that elf actually
        -- moves based on whether one or more other elves want to move to that location.

        elvesAndPossNewLocs = map genPossMove elfLocs

        -- Given a list of moves, if the list has one element, then make the move. If the list has
        -- more than one element, then none of the elves in the list move, and return the location
        -- they are currently at. With each location returned, indicate with a boolean value if
        -- there was a move or not.

        handlePossMovers :: [(GroveLoc, Maybe GroveLoc)] -> [(GroveLoc, Bool)]
        handlePossMovers [] = error "Emtpy list in handlePossMovers."
        handlePossMovers [(_, Just newLoc)] = [(newLoc, True)]
        handlePossMovers xs = map ((, False) . fst) xs

        -- Create a bit vector indicating which of the eight locations around this one has an elf in
        -- it. Use that to see if this elf can move one of the four directions, checking them in the
        -- order they are in in the list dirOrder4.

        genPossMove :: GroveLoc -> (GroveLoc, Maybe GroveLoc)
        genPossMove elfLocation@(x, y)

          -- If there are no elves around the current elf, then don't move this elf.

          | elfBits == 0 = (elfLocation, Nothing)
          | otherwise = (elfLocation, newLocation)
          where
            newLocation = foldr checkClear Nothing dirOrder4

            checkClear (dirBitMask, (fx, fy)) acc
              | elfBits .&. dirBitMask == 0 = Just (fx x, fy y)
              | otherwise = acc

            elfBits = foldl' setBitIfElf 0x0 indsAndBMLocs
            indsAndBMLocs = [((xDec, yInc), nw), ((x, yInc), n), ((xInc, yInc), ne), ((xInc, y), e),
                             ((xInc, yDec), se), ((x, yDec), s), ((xDec, yDec), sw), ((xDec, y), w)]
            xDec = x - 1
            xInc = x + 1
            yDec = y - 1
            yInc = y + 1

            -- Given the index of a spot in the grove in one of the 8 locations next to the elf
            -- location, set the bit corresponding to this location in the accumulator if there is
            -- an elf there.

            setBitIfElf :: Int -> (GroveLoc, Int) -> Int
            setBitIfElf acc (ind@(x1, y1), bt)
              | x1 < gaXMin || x1 > gaXMax = acc
              | y1 < gaYMin || y1 > gaYMax = acc
              | groveArr UA.! ind == Elf = setBit acc bt
              | otherwise = acc

-- Return the rectangular boundary of elves in the grove.

findElfBounds :: [GroveLoc] -> (GroveLoc, GroveLoc)
findElfBounds elfLocs = ((minX, minY), (maxX, maxY))
  where
    minX = (minimum . map fst) elfLocs
    minY = (minimum . map snd) elfLocs
    maxX = (maximum . map fst) elfLocs
    maxY = (maximum . map snd) elfLocs

-- Given the input lines, generate an array to represent the initial grove.

convertInputLinesToArray :: Int -> [String] -> UA.Array GroveLoc GroundContent
convertInputLinesToArray rowLen strs
  = UA.array ((0, 0), (rowLen - 1, rowCount - 1)) arrayInitializationList
  where
    arrayInitializationList = concat $ zipWith addInd convertedInput [0..]
    convertedInput = (reverse . map (map convCharToEnum)) strs
    rowCount = length strs

    addInd :: [GroundContent] -> Int -> [(GroveLoc, GroundContent)]
    addInd rowContents yInd = zipWith (\xInd content -> ((xInd, yInd), content)) [0..] rowContents

convCharToEnum :: Char -> GroundContent
convCharToEnum ch
  | ch == '.' = Empt
  | ch == '#' = Elf
  | otherwise = error ("Unexpected input character: " ++ show ch)

-- Collect and return the elf locations from the given grove array.

getElfLocsFromArr :: GroveArray -> [GroveLoc]
getElfLocsFromArr = map fst . filter ((== Elf) . snd) . UA.assocs

-- The bit location in a bit vector correspoinding to the given location with respect to the current
-- elf.

nw :: Int
nw = 0
n  :: Int
n  = 1
ne :: Int
ne = 2
e  :: Int
e  = 3
se :: Int
se = 4
s  :: Int
s  = 5
sw :: Int
sw = 6
w  :: Int
w  = 7

-- Bit masks representing the three adjoining locations to each of the four directions from the
-- current elf.

northClearBits :: Int
northClearBits = foldl' setBit 0x0 [nw, n, ne]
southClearBits :: Int
southClearBits = foldl' setBit 0x0 [sw, s, se]
eastClearBits :: Int
eastClearBits = foldl' setBit 0x0 [ne, e, se]
westClearBits :: Int
westClearBits = foldl' setBit 0x0 [nw, w, sw]
