-- For Advent of Code 2022
--
-- By Truman Collins
-- December 9, 2022

module Solution_09 (
  puzzle_09
) where

import qualified Data.Array as A
import qualified GHC.Ix as IX
import qualified Data.Set as Set
import Control.Monad
import Parsers

-- The direction of movement. Create an Ix instance for this so we can use an array lookup to get
-- the increment to move one space in a given direction.

data Direction = UpDir | DownDir | LeftDir | RightDir | NoDir
     deriving (Show, Eq, Ord, Enum, Bounded)
instance IX.Ix Direction where
  range (m, n) = [m..n]
  inRange (m, n) i = m <= i && i <= n
  index b i | IX.inRange b i = IX.unsafeIndex b i
            | otherwise = error "Out of range indexing with RPS."
  unsafeIndex (low, _) i = fromEnum i - fromEnum low

type Move = (Direction, Int)
type Position = (Int, Int)
type PosInc = (Int, Int)
type KnotPositions = [Position]

--
-- Code for Puzzle 9.
--

puzzle_09 :: IO (Int, Int)
puzzle_09 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile "puzzle_09.inp")

  when (badParseOfInputLines puzzInp)
       (ioError $ userError "Bad input for puzzle 9.")

  let inputData = map (fst . head) puzzInp
      tailLocSet1 = (Set.fromList . map snd . makeMovesKnotTrain (NoDir, 0) [(0, 0), (0, 0)])
                    inputData
      part1Ans = Set.size tailLocSet1
      tailLocSet2 = (Set.fromList . map snd . makeMovesKnotTrain (NoDir, 0) (replicate 10 (0, 0)))
                    inputData
      part2Ans = Set.size tailLocSet2

  return (part1Ans, part2Ans)

-- Given a single move, a list of knot positions, and a further list of moves, make one step for the
-- current move for the list of knot positions, and repeat until the current move is complete, then
-- move on to the next move in the list until the list is empty. Return a list of pairs containing
-- the knot positions after each step of a move, and the position of the last know.

makeMovesKnotTrain :: Move -> KnotPositions -> [Move] -> [(KnotPositions, Position)]

-- This shouldn't happen, but is for the case where there is a knot train of zero length.

makeMovesKnotTrain _ [] _ = []

-- If the current move has no more steps, and we have no more multi-step moves to work on next, we
-- are done, so return the current knot train positions along with the last position, which is the
-- one we want to track.

makeMovesKnotTrain (_, 0) currPos [] = [(currPos, last currPos)]

-- Here we have worked our way through the current multi-step move, and all we do here is make the
-- next multi-step move the current one.

makeMovesKnotTrain (_, 0) currPos (nextMove : moves) = makeMovesKnotTrain nextMove currPos moves

makeMovesKnotTrain (dir, dist) currPos moves
  = (currPos, lastLoc) : makeMovesKnotTrain (dir, newDist) nextPos moves
  where
    newDist = dist - 1
    (nextPos, lastLoc) = updatePositionsForMove (deltaArr A.! dir) currPos

    updatePositionsForMove :: PosInc -> KnotPositions -> (KnotPositions, Position)

    -- This case should never happen, as there will always be at least one element in the list.

    updatePositionsForMove _ [] = ([], (0, 0))

    updatePositionsForMove posInc [pos] = let newPos = incPos posInc pos
                                              posList = newPos `seq` [newPos]
                                          in  posList `seq` (posList, pos)
    updatePositionsForMove posInc (pos1 : pos2 : poss)
      = let (newPos2Train, lastCaboose) = updatePositionsForMove nextPosInc (pos2 : poss)
            nextPosInc = calcPosIncForNextKnot newPos1 pos2
            newPos1 = incPos posInc pos1
        in  (newPos1 : newPos2Train, lastCaboose)

-- Given a head and tail position of two directly connected knots, where the head has moved and the
-- tail hasn't, calculate the incremental move needed for the tail to follow the head.

calcPosIncForNextKnot :: Position -> Position -> PosInc
calcPosIncForNextKnot hB@(hX, hY) tB@(tX, tY)
  | abs diffX <= 1 && abs diffY <= 1 = (0, 0)
  | diffX ==  2 = (1, yAdjForX2)
  | diffX == -2 = (-1, yAdjForX2)
  | diffY ==  2 = (xAdjForY2, 1)
  | diffY == -2 = (xAdjForY2, -1)
  | otherwise = error ("Unexpected head move: " ++ show (hB, tB) ++ show (diffX, diffY))
  where
    yAdjForX2
      | diffY == 0 =  0
      | diffY >= 1 =  1
      | otherwise  = -1
    xAdjForY2
      | diffX == 0 =  0
      | diffX >= 1 =  1
      | otherwise  = -1
    diffX = hX - tX
    diffY = hY - tY

-- Add the position increment to the position and return the result.

incPos :: PosInc -> Position -> Position
incPos (incX, incY) (x, y) = let newX = x + incX
                                 newY = newX `seq` y + incY
                             in  newY `seq` (newX, newY)

-- The delta incrment to add to a position for each direction of movement.

deltaArr :: A.Array Direction PosInc
deltaArr = A.array (UpDir, NoDir) [(UpDir, (0, 1)), (DownDir, (0, -1)), (LeftDir, (-1, 0)),
                                   (RightDir, (1, 0)), (NoDir, (0, 0))]

-- Parse an input line.

readInputLine :: Parser Move
readInputLine = do
    dir <- letter
    _ <- space
    dist <- int
    let dirEnum = case dir of
                    'L' -> LeftDir
                    'U' -> UpDir
                    'R' -> RightDir
                    'D' -> DownDir
                    _   -> error "Bad direction in puzzle 9 input."
    return (dirEnum, dist)
