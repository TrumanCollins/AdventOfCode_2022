-- For Advent of Code 2022
--
-- By Truman Collins
-- December 11, 2022

module Solution_11 (
  puzzle_11
) where

import Data.List
import Data.Function
import qualified Data.Array as A
import Data.Foldable (toList)
import qualified Data.Sequence as SQ
import Control.Applicative
import Control.Monad
import Parsers

data Op = Plus | Mult deriving (Eq, Show)

data Term = Old | Numb Integer deriving (Eq, Show)

data OpTerm = OpTerm Op Term deriving (Eq, Show)

data MonkeyData = MonkeyData { _id               :: Int
                             , _startWorryLevels :: [Integer]
                             , _worryChange      :: OpTerm
                             , _divisibleBy      :: Integer
                             , _trueThrow        :: Int
                             , _falseThrow       :: Int
                             } deriving Show

type InspCountAndSeq = (Integer, SQ.Seq Integer)
type ItemsAndCountsArr = A.Array Int InspCountAndSeq

--
-- Code for Puzzle 11.
--

puzzle_11 :: IO (Integer, Integer)
puzzle_11 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (parse readAllMonkeyData) (readFile "puzzle_11.inp")

  -- Make sure there was a valid parse of the input.

  when (badParseOfInput puzzInp)
       (ioError $ userError "Invalid input data for puzzle 11.")

  -- Create a sorted (by ID) array of monkey data. Find the least common multiple of all of the
  -- divisors used by the monkeys. We just take the product here because all of these divisors are
  -- prime. Also, create an array holding the current list of items in a sequence, so we can
  -- efficiently add to the end, and a count of the number of items inspected.

  let monkeyList = (sortBy (compare `on` _id) . fst . head) puzzInp
      lowID  = (_id . head) monkeyList
      highID = (_id . last) monkeyList
      lcmVal = (product . map _divisibleBy . A.elems) monkeyArr
      monkeyArr  = A.array (lowID, highID) (map (\m -> (_id m, m)) monkeyList)
      initSeqArr = A.array (lowID, highID) (map genIDAndInitSequence monkeyList)

  -- Iterate through 20 rounds for the first answer. We don't need to for this part, but mod by the
  -- lcm value here as well as in part 2.

  let statesBeforeRoundsP1 = iterate (performFullRound lcmVal True monkeyArr) initSeqArr
      state20 = statesBeforeRoundsP1 !! 20
      part1Ans = (product . take 2 . reverse . sort . map fst . A.elems) state20

  -- Iterate through 1,000 rounds for the second answer. Since the worry level isn't divided by 3
  -- after each inspection, we take the mod value of the worry level of each item at each step using
  -- the lcm of the divisors. This keep the worry levels to a reasonable size and yet with modular
  -- arithmetic, the results are the same.

  let statesBeforeRoundsP2 = iterate (performFullRound lcmVal False monkeyArr) initSeqArr
      state10000 = statesBeforeRoundsP2 !! 10000
      part2Ans = (product . take 2 . reverse . sort . map fst . A.elems) state10000

  return (part1Ans, part2Ans)

-- Given a MonkeyData record, return the id of the monkey, and a pair consisting of zero for the sum
-- of inspections and a sequence with the initial item list.

genIDAndInitSequence :: MonkeyData -> (Int, InspCountAndSeq)
genIDAndInitSequence monkeyData = (_id monkeyData, (0, SQ.fromList (_startWorryLevels monkeyData)))

-- This function will do a full round of monkey business. Given the current state of the list of
-- items held by each monkey (in a sequence), go through them and distribute them by the rules.

performFullRound :: Integer -> Bool -> A.Array Int MonkeyData
                    -> ItemsAndCountsArr -> ItemsAndCountsArr
performFullRound lcmVal worryDiv3 monkeyDataArr inspCountAndSeqArr = finalInspCountAndSeqArr
  where

    -- Process each monkey's list of items from low index to high index.

    finalInspCountAndSeqArr = foldl' performMonkeyActions inspCountAndSeqArr [low..high]
    (low, high) = A.bounds inspCountAndSeqArr

    -- Do one monkey's turn, updating its sequence of items and all of the item sequences that items
    -- were added to.

    performMonkeyActions :: ItemsAndCountsArr -> Int -> ItemsAndCountsArr
    performMonkeyActions inArr index = finalArr
      where

        -- Distribute the items into the array cleared of the current monkey items.

        finalArr = A.accum addToEndOfSeq monkeyClearedArr distributedItems
        distributedItems = map distributeItem (toList itemsToDistribute)

        -- Update the incoming array to have an empty sequence for the current monkey and an updated
        -- inspection count.

        monkeyClearedArr = newCount `seq` inArr A.// [(index, (newCount, SQ.empty))]
        newCount = count + fromIntegral (SQ.length itemsToDistribute)

        -- Values associated with this monkey.

        (count, itemsToDistribute) = inArr A.! index
        monkeyData = monkeyDataArr A.! index

        -- Add the given item to the end of the sequence in this tuple.

        addToEndOfSeq :: InspCountAndSeq -> Integer -> InspCountAndSeq
        addToEndOfSeq (cnt, items) currItem = (cnt, items SQ.|> currItem)

        -- Given the initial worry value for an item, update the worry value, dividing by 3 if
        -- appropriate and modding by the lcm, then return the new worry value and the monkey that
        -- gets it next.

        distributeItem :: Integer -> (Int, Integer)
        distributeItem inWorry = (monkey, outWorry)
          where
            inspectWorry = doOp (_worryChange monkeyData) inWorry
            monkey
              | outWorry `rem` _divisibleBy monkeyData == 0 = _trueThrow monkeyData
              | otherwise = _falseThrow monkeyData
            outWorry = (if worryDiv3 then inspectWorry `quot` 3 else inspectWorry) `rem` lcmVal

        -- Given the current worry value, return the new worry value, which comes from the
        -- worry-change operation. There are very few choices here, so this isn't a full expression
        -- analysis.

        doOp :: OpTerm -> Integer -> Integer
        doOp (OpTerm op trm) currWorry
          | op == Plus = currWorry + otherValue
          | otherwise  = currWorry * otherValue
          where
            otherValue = termValue trm

            termValue Old = currWorry
            termValue (Numb val) = val

-- Parse a number (one or more) of monkey data records.

readAllMonkeyData :: Parser [MonkeyData]
readAllMonkeyData = some readMonkeyRecord

-- Parse a single monkey data record.

readMonkeyRecord :: Parser MonkeyData
readMonkeyRecord = do
  _ <- symbol "Monkey"
  _ <- space
  monkID <- nat
  _ <- symbol ":"
  _ <- space
  _ <- symbol "Starting items: "
  worryLevels <- fmap (map fromIntegral) cslOfNats
  _ <- space
  _ <- symbol "Operation: new = old "
  worryChange <- readOpTerm
  _ <- space
  _ <- symbol "Test: divisible by"
  _ <- space
  divisibleBy <- integerNat
  _ <- space
  _ <- symbol "If true: throw to monkey"
  _ <- space
  trueThrow <- nat
  _ <- space
  _ <- symbol "If false: throw to monkey"
  _ <- space
  falseThrow <- nat
  let validMonkeyRec = MonkeyData monkID worryLevels worryChange divisibleBy trueThrow falseThrow
  return validMonkeyRec

-- Parse an OpTerm, which is either a '+' or '*'.

readOpTerm :: Parser OpTerm
readOpTerm = do
    _ <- symbol "+"
    _ <- space
    OpTerm Plus <$> readTerm
  <|> do
    _ <- symbol "*"
    _ <- space
    OpTerm Mult <$> readTerm

-- Parse either the word 'old' or an integer, and return the appropriate Term.

readTerm :: Parser Term
readTerm = do
    _ <- symbol "old"
    return Old
  <|> do Numb . fromIntegral <$> nat
