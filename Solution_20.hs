-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

{-# Language FlexibleContexts #-}

module Solution_20 (
  puzzle_20
) where

import Data.Int
import qualified Data.Array.ST as STA
import Control.Monad.ST

data MoveDir = LeftDir | RightDir deriving (Show, Eq)


--
-- Code for Puzzle 20.
--

puzzle_20 :: IO (Int64, Int64)
puzzle_20 = do

  let (decryptionKey, nthList, inputFile) = (811589153, [1000, 2000, 3000], "puzzle_20.inp")

  -- Read in the file and convert it to a list of triples, one per line in the file.

  encryptedCode <- fmap (map read . lines) (readFile inputFile)

  let ansPart1 = performDecryptionProcess 1 nthList encryptedCode

  let ansPart2 = performDecryptionProcess 10 nthList (map (* decryptionKey) encryptedCode)

  return (ansPart1, ansPart2)

-- Given the number of decryption cycles, the index list to pic and sum after the 0, and the
-- encrypted list of codes, return the decryption number.

performDecryptionProcess :: Int -> [Int] -> [Int64] -> Int64
performDecryptionProcess cycleCount nthList encryptedCode = result
  where
    result = (sum . map (\nth -> decryptedCodesFromZero !! (nth `rem` codeCount))) nthList
    decryptedCodesFromZero = (dropWhile (/= 0) . cycle) decryptedCodes
    decryptedCodes = runST $ decryptCode encryptedCode cycleCount
    codeCount = length encryptedCode

-- This function takes in an encrypted list and a number indicating the decryption cycles to
-- perform, and it will decrypt the list that many times. We use the ST monad for efficiency here.

decryptCode  :: [Int64] -> Int -> ST s [Int64]
decryptCode initial cycles = do
  let maxInd = length initial - 1
      arrBounds = (0, maxInd) :: (Int, Int)

  -- Create an array that is indexed by the original position of each encrypted code and contains
  -- the current location of it in the currLocArr.

  refArr <- STA.newListArray arrBounds [(0 :: Int)..] :: ST s (STA.STArray s Int Int)
  currLocArr <- STA.newListArray arrBounds (zip initial [0..]) :: ST s (STA.STArray s Int (Int64, Int))

  -- Do the encryption process 'cycles' times.

  mapM_ (moveAllElements arrBounds refArr currLocArr) [1..cycles]

  -- Return the final list of codes in a list.

  finalElems <- STA.getElems currLocArr
  let finalList = map fst finalElems
  return finalList

  where

    -- Perform the decryption operation on each element of the code array.

    moveAllElements :: (Int, Int) -> STA.STArray s Int Int -> STA.STArray s Int (Int64, Int)
                       -> Int -> ST s ()
    moveAllElements bounds refArr currLocArr _
      = mapM_ (moveElement bounds refArr currLocArr) [(fst bounds)..(snd bounds)]

    -- Perform the decryption operation on the given element of the code array indicated by the
    -- index into the ref array.

    moveElement :: (Int, Int) -> STA.STArray s Int Int -> STA.STArray s Int (Int64, Int)
                   -> Int -> ST s ()
    moveElement (lowBound, highBound) refArr currLocArr refIndex = do
      currInd <- STA.readArray refArr refIndex
      (currVal, _) <- STA.readArray currLocArr currInd

      -- Figure out the direction the code will move in the array.

      let dir = if currVal < 0 then LeftDir else RightDir

      -- Do the swap the given number of times, mod the total elements.

      swapElement dir currInd (abs currVal `rem` totalElems64)

        where
          totalElems = highBound - lowBound
          totalElems64 = fromIntegral totalElems :: Int64

          -- The swap element function given a direction, the index of the code to swap, and the
          -- number of swaps. Note that this could be sped up by figuring out the ultimate
          -- destination of the element to be swapped and then shifting the appropriate other
          -- elements.

          swapElement dir srcIndex cnt

            -- Test for done with the swaps.

            | cnt == 0 = return ()

            | otherwise = do

                -- For the swap, figure out the destination index, correcting for wrapping around
                -- the ends of the array.

                let destIndex
                      | dir == LeftDir = if srcIndex == lowBound then highBound else srcIndex - 1
                      | otherwise = if srcIndex == highBound then lowBound else srcIndex + 1

                -- Do the swap in the currLocArr.

                src@(_, srcRefInd)   <- STA.readArray currLocArr srcIndex
                dest@(_, destRefInd) <- STA.readArray currLocArr destIndex
                STA.writeArray currLocArr destIndex src
                STA.writeArray currLocArr srcIndex dest

                -- Do the swap in the ref array.

                STA.writeArray refArr srcRefInd destIndex
                STA.writeArray refArr destRefInd srcIndex

                -- Tail recursive call for the next swap.

                swapElement dir destIndex (cnt - 1)
