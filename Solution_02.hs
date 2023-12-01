-- For Advent of Code 2022
--
-- By Truman Collins
-- December 2, 2022

module Solution_02 (
  puzzle_02
) where

import Data.List
import qualified Data.Array.Unboxed as UA
import qualified Data.Bifunctor as BF
import qualified GHC.Ix as IX
import Control.Applicative
import Control.Monad
import Parsers


--
-- Code for Puzzle 2.
--

-- Enumeration for the three possible plays in this game. Also, define this enumeration for indexing
-- into an array, where we will keep the value of making each play.

data RPS = Rock | Paper | Scissors deriving (Show, Eq, Ord, Enum, Bounded)
instance IX.Ix RPS where
  range (m, n) = [m..n]
  inRange (m, n) i = m <= i && i <= n
  index b i | IX.inRange b i = IX.unsafeIndex b i
            | otherwise = error "Out of range indexing with RPS."
  unsafeIndex (low, _) i = fromEnum i - fromEnum low

-- Enumeration for the three possible hints for player 2. Also, define this enumeration for indexing
-- into an array, where we will keep the conversion for part 1.

data XYZ = X | Y | Z deriving (Show, Eq, Ord, Enum, Bounded)
instance IX.Ix XYZ where
  range (m, n) = [m..n]
  inRange (m, n) i = m <= i && i <= n
  index b i | IX.inRange b i = IX.unsafeIndex b i
            | otherwise = error "Out of range indexing with XYZ."
  unsafeIndex (low, _) i = fromEnum i - fromEnum low

puzzle_02 :: IO (Int, Int)
puzzle_02 = do

  -- Read in the file and convert it to a list of strings, one per line in the file. This input
  -- could be parsed more simply by just splitting by line and picking out the first and third
  -- character of each. Using a parser is a little more complicated, but handles errors more
  -- naturally.

  parseRes <- fmap (parse readRPSData) (readFile "puzzle_02.inp")

  -- Make sure there was a valid parse of the input.

  when (badParseOfInput parseRes)
       (ioError $ userError "Invalid input data for puzzle 2.")

  -- Define the value associated with each play intrinsicly, then the conversions for part 1.

  let convArr  = UA.array (X, Z) [(X, Rock), (Y, Paper), (Z, Scissors)]
      valueArr = UA.array (Rock, Scissors) [(Rock, 1), (Paper, 2), (Scissors, 3)]

  -- Get the list of strategies from the parser results.

  let strategyList = (fst . head) parseRes

  -- Generate the list of plays for part one and play it out to score.

  let streamOfPlaysPart1 = convertSecondToRPS convArr strategyList
      (_, p2ScorePart1)  = playAndScoreStream valueArr streamOfPlaysPart1

  -- Generate the list of plays for part two and play it out to score.

  let streamOfPlaysPart2 = map selectSecondForPart2 strategyList
      (_, p2ScorePart2)  = playAndScoreStream valueArr streamOfPlaysPart2

  return (p2ScorePart1, p2ScorePart2)

--  where

-- Convert the second values in this stream for part 1. Use the conversion array. We could skip
-- this layer of function call and just use the map directly, but this is helpful nailing down
-- the types.

convertSecondToRPS :: UA.Array XYZ RPS -> [(RPS, XYZ)] -> [(RPS, RPS)]
convertSecondToRPS convArr = map (BF.second (convArr UA.!))

-- Given a pair from the input, convert the second value based on the hint.

selectSecondForPart2 :: (RPS, XYZ) -> (RPS, RPS)
selectSecondForPart2 (p1Play, hint) = (p1Play, p2Play)
  where
    p2Play = case p1Play of
               Rock     -> case hint of
                             X -> Scissors
                             Y -> Rock
                             Z -> Paper
               Paper    -> case hint of
                             X -> Rock
                             Y -> Paper
                             Z -> Scissors
               Scissors -> case hint of
                             X -> Paper
                             Y -> Scissors
                             Z -> Rock

-- Play out this stream of moves and accumulate the scores for both players.

playAndScoreStream :: UA.Array RPS Int -> [(RPS, RPS)] -> (Int, Int)
playAndScoreStream valueArr = foldl' playAndScoreOneRound (0, 0)
  where

    -- For an individual play, compute the choice value and win value for each player and add them
    -- to the accumulations for the result.

    playAndScoreOneRound :: (Int, Int) -> (RPS, RPS) -> (Int, Int)
    playAndScoreOneRound (accP1, accP2) (p1Play, p2Play) = newP1 `seq` (newP1, newP2)
      where
        newP1 = newP2 `seq` accP1 + p1WLD + p1PlayVal
        newP2 = accP2 + p2WLD + p2PlayVal
        [p1PlayVal, p2PlayVal] = map (valueArr UA.!) [p1Play, p2Play]
        (p1WLD, p2WLD) = case p1Play of
                           Rock     -> case p2Play of
                                         Rock -> (3, 3)
                                         Paper -> (0, 6)
                                         Scissors -> (6, 0)
                           Paper    -> case p2Play of
                                         Rock -> (6, 0)
                                         Paper -> (3, 3)
                                         Scissors -> (0, 6)
                           Scissors -> case p2Play of
                                         Rock -> (0, 6)
                                         Paper -> (6, 0)
                                         Scissors -> (3, 3)

-- Parse one or more entries and return them in a list.

readRPSData :: Parser [(RPS, XYZ)]
readRPSData = some readRPSLine

-- Parse one line of input, returning a pair holding both enums.

readRPSLine :: Parser (RPS, XYZ)
readRPSLine = do
  rps <- readRPS
  _ <- space
  xyz <- readXYZ
  _ <- space
  return (rps, xyz)

-- Parse a Rock, Paper, Scissors symbol, (A, B, C), and return the corresponding enum.

readRPS :: Parser RPS
readRPS = do
    _ <- symbol "A"
    return Rock
  <|> do
    _ <- symbol "B"
    return Paper
  <|> do
    _ <- symbol "C"
    return Scissors

-- Parse an X, Y, Z symbol and return the corresponding enum.

readXYZ :: Parser XYZ
readXYZ = do
    _ <- symbol "X"
    return X
  <|> do
    _ <- symbol "Y"
    return Y
  <|> do
    _ <- symbol "Z"
    return Z
