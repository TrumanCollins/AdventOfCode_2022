-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_19 (
  puzzle_19
) where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Parallel.Strategies
import Parsers

data Action = NoAction | BuildOreRobot | BuildClayRobot | BuildObsidRobot | BuildGeodeRobot deriving Show

-- This record holds the information for a blueprint.

data Blueprint = Blueprint { indexNum       :: Int
                           , oreRobotCost   :: Int
                           , clayRobotCost  :: Int
                           , obsidRobotCost :: (Int, Int)
                           , geodeRobotCost :: (Int, Int)
                           } deriving (Show, Eq)

-- This record holds the current state of production for a given blueprint, after a given number of
-- hours. There are many possible states after a period of time, so these are stored in a list
-- representing the possible states.

data CurrState = CurrState { blueprint        :: Blueprint
                           , hourCount        :: Int
                           , oreCount         :: Int
                           , clayCount        :: Int
                           , obsidCount       :: Int
                           , geodeCount       :: Int
                           , oreRobotCount    :: Int
                           , clayRobotCount   :: Int
                           , obsidRobotCount  :: Int
                           , geodeRobotCount  :: Int
                           , actions          :: [Action]
                           } deriving Show

instance Eq  CurrState where

  -- When checking equality, don't compare the blueprint, hour, or action list, as these will be
  -- identical, or in the case of the action list, may be different, but end in the same state.

  (==) s1 s2 = and comparisons
    where
      comparisons = map doCmp [oreCount, clayCount, obsidCount, geodeCount, geodeRobotCount,
                               obsidRobotCount, clayRobotCount, oreRobotCount, hourCount]
      doCmp fn = fn s1 == fn s2

instance Ord CurrState where

  -- Compare the two states, ignoring the blueprints, which will be the same, and note that the
  -- first comparison is of the geode counts, and we compare backward, so if a list of states is
  -- sorted, the first one will be with the highest geode count.

  compare s1 s2 = foldr (\b acc -> if b == EQ then acc else b) EQ comparisons
    where
      comparisons = map doCmp [geodeCount, obsidCount, clayCount, oreCount, geodeRobotCount,
                               obsidRobotCount, clayRobotCount, oreRobotCount, hourCount]
      doCmp fn = compare (fn s2) (fn s1)

-- Generate the initial state for a given blueprint. Everything should be initialized to zero except
-- the number of ore robots where we start with one.

genInitialState :: Blueprint -> CurrState
genInitialState bp = CurrState bp 0 0 0 0 0 1 0 0 0 []

--
-- Code for Puzzle 19.
--

puzzle_19 :: IO (Int, Int)
puzzle_19 = do

  let (minuteLimitP1, minuteLimitP2, inputFile) = (24, 32, "puzzle_19.inp")

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure.

  when (badParseOfInputLines puzzInp)
       (ioError $ userError ("Bad input for puzzle 19 from file " ++ inputFile))

  let blueprints = map (fst . head) puzzInp

  -- Parallelizing this doesn't seem to help much, and I don't know why.

  let ansPart1 = (sum . withStrategy (parListChunk 2 rpar)
                  . map (fst . genQualityLevelAndCount minuteLimitP1)) blueprints

  let ansPart2 = (product . parMap rseq (snd . genQualityLevelAndCount minuteLimitP2) . take 3)
                 blueprints

  return (ansPart1, ansPart2)

-- Return the quality level and raw geode count given a number of iterations (minutes), and a set of
-- blueprints.

genQualityLevelAndCount :: Int -> Blueprint -> (Int, Int)
genQualityLevelAndCount minutes bp
  | null allPoss = error ("No possible end states for index " ++ show index ++ " in problem 19.")
  | otherwise = let count = (geodeCount . head) allPoss
                    qualityLevel = index * count
                in  qualityLevel `seq` (qualityLevel, count)
  where
    index = indexNum bp
    allPoss = genStatesAfterNIterations minutes (genInitialState bp)

-- Return the set of states, with the maximum geode count at the front, from the initial state and
-- the number of iterations (hours).

genStatesAfterNIterations :: Int -> CurrState -> [CurrState]
genStatesAfterNIterations iterCount initialState
  = iterate (genNextStatesAndPrune maxOreNeeded) [initialState] !! iterCount
  where
    maxOreNeeded = maximum [oreRobotCost bp, clayRobotCost bp, (fst . obsidRobotCost) bp,
                            (fst . geodeRobotCost) bp]
    bp = blueprint initialState

-- Generate the next set of states from those passed in, and make sure we remove duplicates and
-- filter out any that can't end up with more geodes than the one with the most now can create.

genNextStatesAndPrune :: Int -> [CurrState] -> [CurrState]
genNextStatesAndPrune maxOreNeeded currStates = prunedNextStates
  where
    prunedNextStates
      | null uniqueNextStates = []
      | otherwise = let (bestCandidate : remainder) = uniqueNextStates
                    in  bestCandidate : filter (possiblyCouldSurpass bestCandidate) remainder
    uniqueNextStates = (removeDupsPreserveOrder . sort) allNextStates
    allNextStates = concatMap (getNextStateFromSingleState maxOreNeeded) currStates

-- We are very conservative in testing whether the test state could possibly surpass the reference
-- state in the future. If any of the item counts or the robot counts are larger, and if there are
-- the same number or more than geodes or geodeRobots, then we assume yes.

possiblyCouldSurpass :: CurrState -> CurrState -> Bool
possiblyCouldSurpass referenceState testState
  = (geodeCount testState >= geodeCount referenceState
      || geodeRobotCount testState >= geodeRobotCount referenceState)
    &&
    (oreCount testState > oreCount referenceState
      || clayCount testState > clayCount referenceState
      || obsidCount testState > obsidCount referenceState
      || geodeCount testState > geodeCount referenceState
      || oreRobotCount testState > oreRobotCount referenceState
      || clayRobotCount testState > clayRobotCount referenceState
      || obsidRobotCount testState > obsidRobotCount referenceState
      || geodeRobotCount testState > geodeRobotCount referenceState)

getNextStateFromSingleState :: Int -> CurrState -> [CurrState]
getNextStateFromSingleState maxOreNeeded
                            (CurrState blueP@(Blueprint _ oreRCost clayRCost
                                              (obsidROreCost, obsidRClayCost)
                                              (geodeROreCost, geodeRObsidCost))
                             hourC oreC clayC obsidC geodeC oreRC clayRC obsidRC geodeRC prevActions)

  -- If we can make a geode robot, nothing else we can do will result in a better geode count later,
  -- so only generate this option.
  
  | geodeRobotCanBeBuilt = let newState = newStateGeodeRBuilt
                           in  newState `seq` [newState]

  -- This is more complex. We can just build an obsidian robot safely if we need one, we have enough
  -- resources to build it, we need more obsidian to build a geode robot, and we will have enough
  -- ore next hour to build a geode if we have enough obsidian at that point or even a clay robot..

  | not haveEnoughObsidRobots && obsidRobotCanBeBuilt && obsidC < geodeRObsidCost
    && newOreC >= obsidROreCost + geodeROreCost
    && newOreC >= obsidROreCost + clayRCost = let newState = newStateObsidRBuilt
                                              in  newState `seq` [newState]

  -- If we have enough robots that generate ore, clay, and obsidian that they generate enough each
  -- hour to replenish any use, then don't build any robots.

  | haveEnoughObsidRobots && haveEnoughClayRobots && haveEnoughOreRobots
    = let newState = newStateNoNewRobots
      in  newState `seq` [newState]

  -- If neither of the above two cases are true, then generate any available options where a robot
  -- is built or where we do nothing.

  | otherwise = catMaybes [justAddOre, buildOreRobot, buildClayRobot, buildObsidRobot]

  where

    -- Some useful tests.

    geodeRobotCanBeBuilt  = oreC >= geodeROreCost && obsidC >= geodeRObsidCost
    obsidRobotCanBeBuilt  = oreC >= obsidROreCost && clayC >= obsidRClayCost
    clayRobotCanBeBuilt   = oreC >= clayRCost
    oreRobotCanBeBuilt    = oreC >= oreRCost
    haveEnoughOreRobots   = oreRC >= maxOreNeeded
    haveEnoughClayRobots  = clayRC >= obsidRClayCost
    haveEnoughObsidRobots = obsidRC >= geodeRObsidCost

    -- Updated counts for the next hour.

    newHour   = newOreC `seq` hourC + 1
    newOreC   = newClayC `seq` oreC + oreRC
    newClayC  = newObsidC `seq` clayC + clayRC
    newObsidC = newGeodeC `seq` obsidC + obsidRC
    newGeodeC = geodeC + geodeRC

    -- One would think that if there is a robot to be built, it would always be best to build one
    -- rather than holding off and waiting for resources to accumulate. While that is true for a
    -- geode robot (taken care of in the first guard above), it isn't for the others. It could be,
    -- for example, that by building a clay robot that we don't really need right now, we delay
    -- building an obsidian robot an hour or two later that we really need. Given this, we always
    -- provide an option to wait for resources to accumulate.

    justAddOre :: Maybe CurrState
    justAddOre = let newState = newStateNoNewRobots
                 in  newState `seq` Just newState

    -- There is no need to build another ore robot if we have enough to cover the maximum use each
    -- hour. Otherwise, build one if we have enough resources.
  
    buildOreRobot   :: Maybe CurrState
    buildOreRobot
      | haveEnoughOreRobots = Nothing
      | oreRobotCanBeBuilt = let newState = newStateOreRBuilt
                             in  newState `seq` Just newState
      | otherwise = Nothing

    -- There is no need to build another clay robot if we have enough to cover the maximum use each
    -- hour. Otherwise, build one if we have enough resources.
  
    buildClayRobot  :: Maybe CurrState
    buildClayRobot
      | haveEnoughClayRobots = Nothing
      | clayRobotCanBeBuilt = let newState = newStateClayRBuilt
                              in  newState `seq` Just newState
      | otherwise = Nothing

    -- There is no need to build another obsidian robot if we have enough to cover the maximum use
    -- each hour. Otherwise, build one if we have enough resources.
  
    buildObsidRobot :: Maybe CurrState
    buildObsidRobot
      | haveEnoughObsidRobots = Nothing
      | obsidRobotCanBeBuilt = let newState = newStateObsidRBuilt
                               in  newState `seq` Just newState
      | otherwise = Nothing

    -- Generate a new state not building any new robots.

    newStateNoNewRobots :: CurrState
    newStateNoNewRobots
      = let newActions = newHour `seq` NoAction : prevActions
        in  newActions `seq` CurrState blueP newHour newOreC newClayC newObsidC newGeodeC
                                       oreRC clayRC obsidRC geodeRC newActions

    -- Generate a new state after building an ore robot.

    newStateOreRBuilt :: CurrState
    newStateOreRBuilt
      = let finalOreC  = newHour `seq` newOreC - oreRCost
            finalOreRC = finalOreC `seq` oreRC + 1
            newActions = finalOreRC `seq` BuildOreRobot : prevActions
        in  newActions `seq` CurrState blueP newHour finalOreC newClayC newObsidC newGeodeC
                                       finalOreRC clayRC obsidRC geodeRC newActions

    -- Generate a new state after building a clay robot.

    newStateClayRBuilt :: CurrState
    newStateClayRBuilt
      = let finalOreC   = newHour `seq` newOreC - clayRCost
            finalClayRC = finalOreC `seq` clayRC + 1
            newActions  = finalClayRC `seq` BuildClayRobot : prevActions
        in  newActions `seq` CurrState blueP newHour finalOreC newClayC newObsidC newGeodeC
                                       oreRC finalClayRC obsidRC geodeRC newActions

    -- Generate a new state after building an obsidian robot.

    newStateObsidRBuilt :: CurrState
    newStateObsidRBuilt
      = let finalOreC    = newHour `seq` newOreC - obsidROreCost
            finalClayC   = finalOreC `seq` newClayC - obsidRClayCost
            finalObsidRC = finalClayC `seq` obsidRC + 1
            newActions   = finalObsidRC `seq` BuildObsidRobot : prevActions
        in  newActions `seq` CurrState blueP newHour finalOreC finalClayC newObsidC newGeodeC oreRC
                                       clayRC finalObsidRC geodeRC newActions

    -- Generate a new state after building a geode robot.

    newStateGeodeRBuilt :: CurrState
    newStateGeodeRBuilt
      = let finalOreC    = newHour `seq` newOreC - geodeROreCost
            finalObsidC  = finalOreC `seq` newObsidC - geodeRObsidCost
            finalGeodeRC = finalObsidC `seq` geodeRC + 1
            newActions   = finalGeodeRC `seq` BuildGeodeRobot : prevActions
        in  newActions `seq` CurrState blueP newHour finalOreC newClayC finalObsidC newGeodeC oreRC
                                       clayRC obsidRC finalGeodeRC newActions

-- Read a line from the input describing a blueprint. Return a record for it.

readInputLine :: Parser Blueprint
readInputLine = do
  _ <- symbol "Blueprint "
  index <- int
  _ <- symbol ": Each ore robot costs "
  oreRobCost <- int
  _ <- symbol "ore. Each clay robot costs "
  clayRobCost <- int
  _ <- symbol "ore. Each obsidian robot costs "
  obsRobOreCost <- int
  _ <- symbol "ore and "
  obsRobClayCost <- int
  _ <- symbol "clay. Each geode robot costs "
  geodeRobOreCost <- int
  _ <- symbol "ore and "
  geodeRobObsCost <- int
  _ <- symbol "obsidian."
  return (Blueprint index oreRobCost clayRobCost (obsRobOreCost, obsRobClayCost)
                    (geodeRobOreCost, geodeRobObsCost))

-- This function removes the duplicates in the list leaving them in the same order.
-- It is notably faster than (map head . group).

removeDupsPreserveOrder :: (Eq a) => [a] -> [a]
removeDupsPreserveOrder [] = []
removeDupsPreserveOrder (x : xs) = x : compress x xs
  where
    compress _ [] = []
    compress z (y : ys)
      | y == z = compressRest
      | otherwise = y : compressRest
      where compressRest = compress y ys
