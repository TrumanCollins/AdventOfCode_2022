-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

{-# Language FlexibleContexts #-}

module Solution_18 (
  puzzle_18
) where

import Data.List
import qualified Data.Array as A
import qualified Data.Array.ST as STA
import Control.Monad
import Control.Monad.ST
import Parsers

data Contents2 = Lava2 | Pocket2 | Empt2 deriving (Eq, Show)
type CaveIndex = (Int, Int, Int)
type CaveBounds = (CaveIndex, CaveIndex)
type CaveArray = A.Array CaveIndex Contents2

--
-- Code for Puzzle 18.
--

puzzle_18 :: IO (Int, Int)
puzzle_18 = do

  let inputFile = "puzzle_18.inp"

  -- Read in the file and convert it to a list of triples, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure.

  when (badParseOfInputLines puzzInp)
       (ioError $ userError ("Bad input for puzzle 18 from file " ++ inputFile))

  let lavaLocs = map (fst . head) puzzInp

  let ((minX, minY, minZ), (maxX, maxY, maxZ))
        = foldl' updateMinMax ((maxBound, maxBound, maxBound), (minBound, minBound, minBound)) lavaLocs
      arrBounds = ((minX - 1, minY - 1, minZ - 1), (maxX + 1, maxY + 1, maxZ + 1))

  let areaArr2 :: CaveArray
      areaArr2 = runST $ buildCaveArray arrBounds lavaLocs

  let ansPart1 = foldl' (accumOpenSideCount2 (/= Lava2) areaArr2) 0 lavaLocs
  let ansPart2 = foldl' (accumOpenSideCount2 (== Empt2) areaArr2) 0 lavaLocs

  return (ansPart1, ansPart2)

buildCaveArray :: CaveBounds -> [CaveIndex] -> ST s CaveArray
buildCaveArray caveBounds@((minX, minY, minZ), (maxX, maxY, maxZ)) lavaLocs = do
  caveArray <- STA.newArray caveBounds Pocket2 :: ST s (STA.STArray s CaveIndex Contents2)

  let -- Function to write Lava into the given location in the array.
      putLava loc = STA.writeArray caveArray loc Lava2

  -- Put all of the lava into the array from the list passed in.

  mapM_ putLava lavaLocs

  -- These will be all of the edge locations around the lava. These locatios and those connected
  -- to them not holding lava are all considered empty.
 
  let edgeLocs = concat $ [[(minX, y, z), (maxX, y, z)] | y <- [minY..maxY], z <- [minZ..maxZ]]
                 ++ [[(x, maxY, z), (x, maxY, z)] | x <- [minX..maxX], z <- [minZ..maxZ]]
                 ++ [[(x, y, minZ), (x, y, maxZ)] | y <- [minY..maxY], x <- [minX..maxX]]

  let fillPocketWithEmpty loc = do
        contents <- STA.readArray caveArray loc
        when (contents == Pocket2)
          $ do STA.writeArray caveArray loc Empt2
               let allNeighbors = genAdjacent loc
                   neighbors = filter notOutsideArray allNeighbors
               mapM_ fillPocketWithEmpty neighbors
        where

          -- Function that returns true if the given location is inside the array bounds.

          notOutsideArray (x1, y1, z1)
            | x1 < minX = False
            | x1 > maxX = False
            | y1 < minY = False
            | y1 > maxY = False
            | z1 < minZ = False
            | z1 > maxZ = False
            | otherwise = True


  mapM_ fillPocketWithEmpty edgeLocs
  STA.freeze caveArray
  
--  returnArr <- STA.freeze caveArray
--  return returnArr
  
-- Accumulate the number of open side of laval blocks.

accumOpenSideCount2 :: (Contents2 -> Bool) -> A.Array CaveIndex Contents2 -> Int -> CaveIndex -> Int
accumOpenSideCount2 countFn areaArr accCount loc = newCount
  where
    newCount = accCount + (length . filter countFn . map (areaArr A.!)) adjacent
    adjacent = genAdjacent loc

-- Generate the adjacent locations.

genAdjacent :: CaveIndex -> [CaveIndex]
genAdjacent (x, y, z) = [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1)]

-- Accumulate the minimum and maximum x, y, and z values.

updateMinMax :: ((Int, Int, Int), (Int, Int, Int)) -> (Int, Int, Int)
                -> ((Int, Int, Int), (Int, Int, Int))
updateMinMax ((accMinX, accMinY, accMinZ), (accMaxX, accMaxY, accMaxZ))
             (currX, currY, currZ)
  = newMinX `seq` ((newMinX, newMinY, newMinZ), (newMaxX, newMaxY, newMaxZ))
  where
    newMinX = newMaxX `seq` min accMinX currX
    newMaxX = newMinY `seq` max accMaxX currX
    newMinY = newMaxY `seq` min accMinY currY
    newMaxY = newMinZ `seq` max accMaxY currY
    newMinZ = newMaxZ `seq` min accMinZ currZ
    newMaxZ = max accMaxZ currZ

-- Given one line of input, parse it and return the result as a RawInput.

readInputLine :: Parser (Int, Int, Int)
readInputLine = do
  x <- int
  _ <- symbol ","
  y <- int
  _ <- symbol ","
  z <- int
  return (x, y, z)
