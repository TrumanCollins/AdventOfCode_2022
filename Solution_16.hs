-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

-- I used a breath-first search for this problem, which worked great for part one and took very
-- little time. I did quite a bit of pruning of the states in the surge, which was very helpful.

-- Part two was harder to do this way because of the increased number of states and also, the code
-- was enough different having two explorers from part one that I wasn’t able to share much code.

-- The solution also took longer than it should and I decided not to try to find a better solution,
-- but here are some ideas. Try removing the rooms from the search with the zero rate and update the
-- paths between the remaining rooms to take into account the shortest path. This should greatly
-- reduce the number of states to keep track of, especially in part two. One interesting thing in
-- this problem is that sometimes it is not the best choice to open the valve when you go into a
-- room where it is closed. That takes one time unit to do it, and if the valve doesn’t release much
-- pressure, it may have been better to move forward to a valve that can release more pressure.

{-# Language TupleSections #-}

module Solution_16 (
  puzzle_16
) where

import Data.List
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Function
import qualified Data.Array as A
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Parsers

-- Stores the data about each valve room.

data ValveData = ValveData { _id         :: Int
                           , _roomName   :: String
                           , _flowRate   :: Int
                           , _tunnelsTo  :: [Int]
                           } deriving Show

-- Holds a path state for part 1.

data BFSState1 = BFSState1 { _loc               :: Int
                           , _lastAction        :: Action
                           , _totalReleased1    :: Int
                           , _valvesOpen1       :: Int64
                           , _valveClosedRates1 :: ClosedValveRates
                           , _priorLocations1   :: PathAndActions1
                           } deriving Show

-- Holds a path state for part 2.

data BFSState2 = BFSState2 { _locH              :: Int
                           , _locE              :: Int
                           , _lastActionH       :: Action
                           , _lastActionE       :: Action
                           , _totalReleased2    :: Int
                           , _valvesOpen2       :: Int64
                           , _valveClosedRates2 :: ClosedValveRates
                           , _priorLocations2   :: PathAndActions2
                           } deriving Show

type RawInput = (String, Int, [String])
type NameIndexMap = M.Map String Int
type ValveDataArray = A.Array Int ValveData
type ClosedValveRates = [Int]
data Action = Start | Tunnel | SwitchOn | Sit deriving (Show, Eq)
type PathAndActions1 = [(Int, Action)]
type PathAndActions2 = [(Int, Action, Int, Action)]
type HumanOrElephantAct = (Int, Action, Maybe Int)

--
-- Code for Puzzle 16.
--

puzzle_16 :: IO (Int, Int)
puzzle_16 = do

  let (inputFile, timeAvail1, timeAvail2, initialRoom) = ("puzzle_16.inp", 30, 26, "AA")

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure.

  when (badParseOfInputLines puzzInp)
       (ioError $ userError ("Bad input for puzzle 18 from file " ++ inputFile))

  let inputData = (zip [0..] . map (fst . head)) puzzInp
      roomCount = length inputData

  -- Make sure there is at least one line of input.
  
  when (roomCount == 0)
       (ioError $ userError ("Empty input for puzzle 16 from file " ++ inputFile))

  -- Set up some helpful data structures for this problem.
  
  let nameToIndMap = M.fromList $ map (\(index, (name, _, _)) -> (name, index)) inputData
      valveDataArr = A.listArray (0, roomCount - 1) (map (genValveData nameToIndMap) inputData)
      valveRates   = (initClosedValves . map _flowRate . A.elems) valveDataArr
      allGT0Valves = (map fst . filter ((> 0) . _flowRate . snd) . A.assocs) valveDataArr
      allGT0ValvesBits = foldl' setBit 0 allGT0Valves
      startValveIndM = M.lookup initialRoom nameToIndMap
      startValveInd = fromJust startValveIndM

  -- Make sure the starting room name is a valid name.

  when (isNothing startValveIndM)
       (ioError $ userError ("Invalid initial room name: " ++ initialRoom))

  -- Solve part 1 with a breadth-first search, where each set is pruned of unneeded states.

  let initialBFSState1 = BFSState1 startValveInd Start 0 0 valveRates []
      bfsSets1 = iterate (genNextStates1 allGT0ValvesBits valveDataArr timeAvail1)
                         (1, [initialBFSState1])
      part1Ans = (_totalReleased1 . head . snd) (bfsSets1 !! (timeAvail1 - 1))

  -- Solve part 2 with a breadth-first search, where each set is pruned of unneeded states.

  let initialBFSState2 = BFSState2 startValveInd startValveInd Start Start 0 0 valveRates []
      bfsSets2 = iterate (genNextStates2 allGT0ValvesBits valveDataArr timeAvail2)
                         (1, [initialBFSState2])
      part2Ans = (_totalReleased2 . head . snd) (bfsSets2 !! (timeAvail2 - 1))

  return (part1Ans, part2Ans)

-- Functions to access the remaining closed valve rates.

-- Create a closed valves data structure from a list of valve values. Only retain postive ones, and
-- put them in reverse order.

initClosedValves :: [Int] -> ClosedValveRates
initClosedValves = sortBy (flip compare) . filter (> 0)

-- Remove a rate from the list. The rate must be in the list and the list must be in reverse order.

removeRate :: Int -> ClosedValveRates -> ClosedValveRates
removeRate _ [] = error "Tried to remove rate not in closed valves."
removeRate val (x : xs)
  | val == x = xs
  | val <  x = x : removeRate val xs
  | otherwise = error "Tried to remove rate not in closed valves."

-- Create a ValveData from the raw input.

genValveData :: NameIndexMap -> (Int, RawInput) -> ValveData
genValveData nameMap (index, (name, flowRate, tunnelsTo))
  = ValveData index name flowRate (mapMaybe (`M.lookup` nameMap) tunnelsTo)

-- Given a list of states and the current minute, generate the list of states in the next
-- minute. Filter this list for states that can't become the state at the end with the most pressure
-- released. This greatly speeds up the process.

genNextStates1 :: Int64 -> ValveDataArray -> Int -> (Int, [BFSState1]) -> (Int, [BFSState1])
genNextStates1 allValvesGT0 valveDataArr totalTimeAvail (currMin, currStatesTop)
  = (nextMinute, nextStates)
  where
    nextStates    = filterAndPruneStates nextStatesAll
    nextStatesAll = concatMap genNext currStatesTop
    nextMinute    = currMin + 1

     -- Takes in a list of next states and filters the list throwing out states that either are at
     -- the same location with the same valves open or that won't be able to catch up to the best so
     -- far.

    filterAndPruneStates :: [BFSState1] -> [BFSState1]
    filterAndPruneStates [] = []
    filterAndPruneStates currStates = fullyWeeded
      where

        fullyWeeded = bestSoFar : filter (canCatchUp mostPressureReleased) weededSitting

        -- Remove any states where we have opened all of the valves and the total released is less
        -- than the state with the most released at this stage.

        weededSitting = filter ((/= Sit) . _lastAction) rest
        mostPressureReleased = _totalReleased1 bestSoFar
        (bestSoFar : rest) = sortBy (flip compare `on` _totalReleased1) weededSameValvesAndLoc

        -- Group together the states that have the same valves open and the same index, and only
        -- keep the one with the largest total released value.

        weededSameValvesAndLoc = keepMaxTotalOfStatesWithSameLocAndValvesOpen currStates

        -- Given the current state, if the remaining closed values were lined up in a row starting
        -- at this location, what would be the maximum pressure released at the end of the time. If
        -- this isn't better than the goal value, which is the best of the current states, then we
        -- don't need to keep considering this state.

        canCatchUp :: Int -> BFSState1 -> Bool
        canCatchUp goal currState = maxPossFromThisState > goal
          where
            maxPossFromThisState = _totalReleased1 currState + maxPossInFuture
            maxPossInFuture  = (sum . zipWith totalPressure minuteRange) closedVRates
            closedValveRates = _valveClosedRates1 currState
            (minuteRange, closedVRates)
              | flowRate > 0 && (not . testBit (_valvesOpen1 currState)) index
                = ([nextMinute, (nextMinute + 2)..(totalTimeAvail - 1)],
                   flowRate : removeRate flowRate closedValveRates)
              | otherwise = ([(nextMinute + 1), (nextMinute + 3)..(totalTimeAvail - 1)],
                             closedValveRates)
            index = _loc currState
            currValve = valveDataArr A.! index
            flowRate = _flowRate currValve

            totalPressure :: Int -> Int -> Int
            totalPressure minute rate = rate * (totalTimeAvail - minute)

        -- Sort by location and valves open, then for each group where these characteristics are the
        -- same keep the one with the largest total pressure released.

        keepMaxTotalOfStatesWithSameLocAndValvesOpen :: [BFSState1] -> [BFSState1]
        keepMaxTotalOfStatesWithSameLocAndValvesOpen
          = map (minimumBy (flip compare `on` _totalReleased1))
            . groupBy eqValvesAndLoc . sortBy cmpValvesAndLoc

        -- Return true if the open valves and location are the same in these states.

        eqValvesAndLoc :: BFSState1 -> BFSState1 -> Bool
        eqValvesAndLoc state1 state2 = _valvesOpen1 state1 == _valvesOpen1 state2
                                       && _loc state1 == _loc state2

        -- Compare two states using first the bits representing the open valves in this state, and
        -- second, the location where we are now.

        cmpValvesAndLoc :: BFSState1 -> BFSState1 -> Ordering
        cmpValvesAndLoc state1 state2
          | cmpValvesOpen == EQ = compare (_loc state1) (_loc state2)
          | otherwise = cmpValvesOpen
          where
            cmpValvesOpen = compare (_valvesOpen1 state1) (_valvesOpen1 state2)

    -- Given a state, generate all next states one minute later.

    genNext :: BFSState1 -> [BFSState1]
    genNext curr@(BFSState1 index lastAction totalReleased valvesOpen closedValveRates priorLocs)

      -- If the last action was to sit, we know all valves are open and the current state doesn't
      -- need to be updated, so just give the current state as the only next state.

      | lastAction == Sit = [curr]

      -- If all of the valves are open, we must have just opened the last one in the last minute, so
      -- the single follow-on state to return is the current state with the prior locations updated
      -- to hold the last valve turn-on.

      | valvesOpen == allValvesGT0 = let finalPath = (index, Sit) : newPath
                                     in [BFSState1 index Sit totalReleased valvesOpen [] finalPath]

      -- If the valve in the current location is off and it has a non-zero flow rate, it can be
      -- turned on, in which case we stay in this room. We also have to include all of the moves
      -- where the valve is not turned on. One might think that you would always want to turn on a
      -- valve if you are in a room with the valve off and where the valve can release pressure, but
      -- it may be better to move on to a different room faster which can release more pressure,
      -- then later return to this one.

      | flowRate > 0 && (not . testBit valvesOpen) index
        = let newOpen     = newPath `seq` setBit valvesOpen index
              newTotalRel = newOpen `seq` totalReleased + flowRate * (totalTimeAvail - currMin)
              newClosedRates = removeRate flowRate closedValveRates
              stateValveOn = newTotalRel `seq` BFSState1 index SwitchOn newTotalRel newOpen
                                                         newClosedRates newPath
          in  stateValveOn `seq` stateValveOn : movesThroughTunnels

      -- Here, either the valve is open or the pressure released from it would be zero. Generate
      -- moves to all adjacent locations.

      | otherwise = movesThroughTunnels

      where
        movesThroughTunnels = map followTunnel adjacents
        currValve = valveDataArr A.! index
        flowRate  = _flowRate currValve
        adjacents = _tunnelsTo currValve
        newPath   = (index, lastAction) : priorLocs

        -- Create a new state from following a tunnel to the given index.

        followTunnel :: Int -> BFSState1
        followTunnel newIndex = BFSState1 newIndex Tunnel totalReleased valvesOpen
                                          closedValveRates newPath

-- Given a list of states and the current minute, generate the list of states in the next
-- minute. Filter this list for states that can't become the state at the end with the most pressure
-- released. This greatly speeds up the process.

genNextStates2 :: Int64 -> ValveDataArray -> Int -> (Int, [BFSState2]) -> (Int, [BFSState2])
genNextStates2 allValvesGT0 valveDataArr totalTimeAvail (currMin, currStatesTop)
  = (nextMinute, nextStates)
  where
    nextStates    = filterAndPruneStates nextStatesAll
    nextStatesAll = concatMap genNext currStatesTop
    nextMinute    = currMin + 1

     -- Takes in a list of next states and filters the list throwing out states that either are at
     -- the same location with the same valves open or that won't be able to catch up to the best so
     -- far.

    filterAndPruneStates :: [BFSState2] -> [BFSState2]
    filterAndPruneStates [] = []
    filterAndPruneStates currStates = fullyWeeded
      where

        fullyWeeded = bestSoFar : filter (canCatchUp mostPressureReleased) weededSitting

        -- Remove any states where we have opened all of the valves and the total released is less
        -- than the state with the most released at this stage.

        weededSitting = filter ((/= Sit) . _lastActionH) rest
        mostPressureReleased = _totalReleased2 bestSoFar
        (bestSoFar : rest) = sortBy (flip compare `on` _totalReleased2) weededSameValvesAndLoc

        -- Group together the states that have the same valves open and the same index, and only
        -- keep the one with the largest total released value.

        weededSameValvesAndLoc = keepMaxTotalOfStatesWithSameLocAndValvesOpen currStates

        -- Given the current state, if the remaining closed values were lined up in a row starting
        -- at this location, what would be the maximum pressure released at the end of the time. If
        -- this isn't better than the goal value, which is the best of the current states, then we
        -- don't need to keep considering this state.

        canCatchUp :: Int -> BFSState2 -> Bool
        canCatchUp goal currState = maxPossFromThisState > goal
          where
            maxPossFromThisState = _totalReleased2 currState + maxPossInFuture
            maxPossInFuture = totalPressure nextMinute initialAdd
                              + (sum . zipWith totalPressure minuteRange) updatedClosedRates
            (minuteRange, initialAdd, updatedClosedRates)
              | sittingOnValveToOpenH
                = if sittingOnValveToOpenE && indexH /= indexE then
                    ([(nextMinute + 2), (nextMinute + 4)..(totalTimeAvail - 1)],
                     flowRateH + flowRateE,
                     doubleUp (removeRate flowRateE (removeRate flowRateH closedRates)))
                  else ([(nextMinute + 1), (nextMinute + 3)..(totalTimeAvail - 1)], flowRateH,
                        removeRate flowRateH closedRates)
              | sittingOnValveToOpenE = ([(nextMinute + 1), (nextMinute + 3)..(totalTimeAvail - 1)],
                                         flowRateE, removeRate flowRateE closedRates)
              | otherwise = ([(nextMinute + 1), (nextMinute + 3)..(totalTimeAvail - 1)], 0,
                             doubleUp closedRates)
            indexH = _locH currState
            indexE = _locE currState
            currValveH  = valveDataArr A.! indexH
            currValveE  = valveDataArr A.! indexE
            flowRateH   = _flowRate currValveH
            flowRateE   = _flowRate currValveE
            valvesOpen  = _valvesOpen2 currState
            closedRates = _valveClosedRates2 currState
            sittingOnValveToOpenH = flowRateH > 0 && (not . testBit valvesOpen) indexH
            sittingOnValveToOpenE = flowRateE > 0 && (not . testBit valvesOpen) indexE

            totalPressure :: Int -> Int -> Int
            totalPressure minute rate = rate * (totalTimeAvail - minute)

            doubleUp :: [Int] -> [Int]
            doubleUp [] = []
            doubleUp [x] = [x]
            doubleUp (x1 : x2 : xs) = (x1 + x2) : doubleUp xs

        -- Sort by location and valves open, then for each group where these characteristics are the
        -- same keep the one with the largest total pressure released.

        keepMaxTotalOfStatesWithSameLocAndValvesOpen :: [BFSState2] -> [BFSState2]
        keepMaxTotalOfStatesWithSameLocAndValvesOpen
          = map (minimumBy (flip compare `on` _totalReleased2))
            . groupBy eqValvesAndLocs . sortBy cmpValvesAndLocs

        -- Return true if the open valves and location are the same in these states.

        eqValvesAndLocs :: BFSState2 -> BFSState2 -> Bool
        eqValvesAndLocs state1 state2 = _valvesOpen2 state1 == _valvesOpen2 state2
                                        && _locH state1 == _locH state2
                                        && _locE state1 == _locE state2

        -- Compare two states using first the bits representing the open valves in this state, and
        -- second, the location where we are now.

        cmpValvesAndLocs :: BFSState2 -> BFSState2 -> Ordering
        cmpValvesAndLocs state1 state2
          | cmpValvesOpen /= EQ = cmpValvesOpen
          | cmpLocH /= EQ = cmpLocH
          | otherwise = compare (_locE state1) (_locE state2)
          where
            cmpValvesOpen = compare (_valvesOpen2 state1) (_valvesOpen2 state2)
            cmpLocH = compare (_locH state1) (_locH state2)

    -- Given a state, generate all next states one minute later.

    genNext :: BFSState2 -> [BFSState2]
    genNext curr@(BFSState2 indexH indexE lastActionH lastActionE totalReleased valvesOpen
                            closedValveRates priorLocs)

      -- If the last action for the human (or the elephant) was to sit, we know all valves are open
      -- and the current state doesn't need to be updated, so just give the current state as the
      -- only next state.

      | lastActionH == Sit = [curr]

      -- If all of the valves are open, we must have just opened the last one in the last minute, so
      -- the single follow-on state to return is the current state with the prior locations updated
      -- to hold the last valve turn-on.

      | valvesOpen == allValvesGT0
        = let finalPath = (indexH, Sit, indexE, Sit) : newPath
          in [BFSState2 indexH indexE Sit Sit totalReleased valvesOpen [] finalPath]

      -- Here, either the valve is open or the pressure released from it would be zero. Generate
      -- moves to all adjacent locations.

      | otherwise = movesThroughTunnels

      where
        movesThroughTunnels
          = [combineBothActsForNewState hAct eAct | hAct <- humanActs, eAct <- elephantActs,
                                                    notOpeningSameValve hAct eAct]
        humanActs
          | atValveToOpenH = (indexH, SwitchOn, Just flowRateH) : humanTunnels
          | otherwise = humanTunnels
          where humanTunnels = map (, Tunnel, Nothing) adjacentsH
        elephantActs
          | atValveToOpenE = (indexE, SwitchOn, Just flowRateE) : elephantTunnels
          | otherwise = elephantTunnels
          where elephantTunnels = map (, Tunnel, Nothing) adjacentsE
        atValveToOpenH = flowRateH > 0 && (not . testBit valvesOpen) indexH
        atValveToOpenE = flowRateE > 0 && (not . testBit valvesOpen) indexE
        currValveH = valveDataArr A.! indexH
        currValveE = valveDataArr A.! indexE
        flowRateH  = _flowRate currValveH
        flowRateE  = _flowRate currValveE
        adjacentsH = _tunnelsTo currValveH
        adjacentsE = _tunnelsTo currValveE
        newPath    = (indexH, lastActionH, indexE, lastActionE) : priorLocs

        combineBothActsForNewState :: HumanOrElephantAct -> HumanOrElephantAct -> BFSState2
        combineBothActsForNewState (indH, actH, valveMH) (indE, actE, valveME)
          | actH == Tunnel && actE == Tunnel
            = BFSState2 indH indE actH actE totalReleased valvesOpen closedValveRates newPath
          | actH == Tunnel
            = let rateOpened = fromJust valveME
                  newValvesOpen = setBit valvesOpen indE
                  newTotalRel = totalReleased + rateOpened * (totalTimeAvail - currMin)
                  newClosedRates = removeRate rateOpened closedValveRates
              in  BFSState2 indH indE actH actE newTotalRel newValvesOpen newClosedRates newPath
          | actE == Tunnel
            = let rateOpened = fromJust valveMH
                  newValvesOpen = setBit valvesOpen indH
                  newTotalRel = totalReleased + rateOpened * (totalTimeAvail - currMin)
                  newClosedRates = removeRate rateOpened closedValveRates
              in  BFSState2 indH indE actH actE newTotalRel newValvesOpen newClosedRates newPath
          | otherwise
            = let rateOpenedH = fromJust valveMH
                  rateOpenedE = fromJust valveME
                  newValvesOpen = setBit (setBit valvesOpen indH) indE
                  newTotalRel = totalReleased + (rateOpenedH + rateOpenedE) * (totalTimeAvail - currMin)
                  newClosedRates = removeRate rateOpenedE (removeRate rateOpenedH closedValveRates)
              in  BFSState2 indH indE actH actE newTotalRel newValvesOpen newClosedRates newPath

        notOpeningSameValve :: HumanOrElephantAct -> HumanOrElephantAct -> Bool
        notOpeningSameValve (indH, actH, _) (indE, actE, _)
          | indH == indE && actH == SwitchOn && actE == SwitchOn = False
          | otherwise = True
        
-- Given one line of input, parse it and return the result as a RawInput.

readInputLine :: Parser RawInput
readInputLine = do
  _ <- symbol "Valve "
  valveName <- identAlpha
  _ <- symbol "has flow rate="
  flowRate <- nat
  _ <- oneOfTwoTunnelPhrases
  tunnelsToValves <- cslOfIdents
  return (valveName, flowRate, tunnelsToValves)
  where
    oneOfTwoTunnelPhrases :: Parser String
    oneOfTwoTunnelPhrases = do
        symbol "; tunnels lead to valves "
      <|> do
        symbol "; tunnel leads to valve "
