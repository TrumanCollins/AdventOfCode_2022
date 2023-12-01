-- For Advent of Code 2022
--
-- By Truman Collins
-- December 6, 2022

module Solution_06 (
  puzzle_06
) where

import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Sequence as SQ
import Control.Monad

--
-- Code for Puzzle 6.
--

-- This function will walk through the string, keeping a window fullLen wide that is tracked by both
-- a map and a sequence and a counter of the number of current duplicates. It will return the index
-- of the character that leads the earliest window of length fullLen with no duplicates..

scanForFirstNNoDups :: Int -> String -> Maybe Int
scanForFirstNNoDups fullLen = go 0 0 SQ.empty M.empty
  where
    go :: Int -> Int -> SQ.Seq Char -> M.Map Char Int -> String -> Maybe Int

    -- If we are out of characters, then we didn't find a solution.

    go _ _ _ _ [] = Nothing

    go lastCharCnt currDup currSeq currMap (c : cs)

      -- We haven't accumulated enouch characters yet, so add this one to the Sequence and the Map,
      -- and move on.

      | SQ.length currSeq < fullLen
        = let newSeq = c SQ.<| currSeq
              (newDup, newMap) = addToMapAndUpdateDupCnt c currDup currMap
          in  currCharCnt `seq` go currCharCnt newDup newSeq newMap cs

      -- If the window is the right size, and there are no duplicates in the window, the last
      -- character count is the answer.

      | currDup == 0 = Just lastCharCnt

      -- Move the window one charactr. Add and remove from the sequence in one operation, but the
      -- removal from the map takes longer. We need to add to it, then remove from it, and do a
      -- lookup in the middle to see if what we are removing is a duplicate.

      | otherwise
        = let (newSeq SQ.:|> x) = c SQ.<| currSeq
              (tempDup, tempMap) = addToMapAndUpdateDupCnt c currDup currMap
              charCnt = fromJust $ M.lookup x tempMap
              newMap  = M.update decCountOrRemove x tempMap
              newDup = currCharCnt `seq` if charCnt == 1 then tempDup else tempDup - 1
          in  newDup `seq` go currCharCnt newDup newSeq newMap cs
      where
        currCharCnt = lastCharCnt + 1

        -- Add together the two entries. The second will always be 1.
  
        addTwo :: Char -> Int -> Int -> Int
        addTwo _ v1 v2 = v1 + v2

        -- Used to remove a character from the map. Decrement the count if over one, otherwise
        -- remove it.
  
        decCountOrRemove :: Int -> Maybe Int
        decCountOrRemove val = if val == 1 then Nothing else Just (val - 1)

        -- Add the given character to the map and update the duplicate count passed in.
  
        addToMapAndUpdateDupCnt :: Char -> Int -> M.Map Char Int -> (Int, M.Map Char Int)
        addToMapAndUpdateDupCnt ch dupCnt mapIn
          = let (oldValM, updatedMap) = M.insertLookupWithKey addTwo  ch 1 mapIn
                updatedDup = if isNothing oldValM then dupCnt else dupCnt + 1
            in  updatedDup `seq` (updatedDup, updatedMap)

puzzle_06 :: IO (Int, Int)
puzzle_06 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- readFile "puzzle_06.inp"

  let noDupsIndP1M = scanForFirstNNoDups 4 puzzInp
      noDupsIndP2M = scanForFirstNNoDups 14 puzzInp

  when (isNothing noDupsIndP1M)
       (ioError $ userError "No answer for part 1 of puzzle 6.")

  let noDupsIndP1 = fromJust noDupsIndP1M
      noDupsIndP2 = fromJust noDupsIndP2M

  return (noDupsIndP1, noDupsIndP2)
