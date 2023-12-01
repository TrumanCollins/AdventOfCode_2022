-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_22 (
  puzzle_22
) where

-- This was a challenging one, in particular part two. I solved part one pretty quickly and then
-- once I saw part two, altered my solution to part one to be more workable for solving part two as
-- well. The general idea is to move around the map, which is stored in a two dimensional array, and
-- pair that map with a move array that, for each location, indicates the next location and
-- direction when moving in the four directions, North, East, South, and West. In part one, this
-- just works out to wrapping around the map when we run out of bounds or into a void section. In
-- part two, it involves wrapping around the cube. Figuring out what those moves and new directions
-- are was quite challenging with a lot of detail.

-- I generated first a tile array, which has one boolean element per tile or face. Initially, this
-- is just true for tiles that are faces and false for void tiles. I then used this to create a new
-- tile array. I figure out where I want to place the top phase, based on number of neighbors and
-- centrallity, and then iterate defining faces and orientations radiating out from there.

-- Given this information, I generated the move array so that at each edge of the map or where a
-- void tile is, there is a calculation of what face tile we are currently on, what the next tile we
-- would expect removing off that tile in the given direction is, and finally what specific location
-- along that edge and what the new direction is on the map. Because the move array is a boxed
-- array, these calculations only actually happen for those squares that the path actually moves
-- through. Once all this code was working, the solution is very fast.

import Data.List
import Data.Char
import Data.Function
import Data.Bifunctor
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import qualified GHC.Ix as IX
import Utilities

-- Define Facing and Face types, and because both are used to index array, define instances of Ix
-- for each.

data Facing = North | East | South | West deriving (Show, Eq, Enum, Bounded, Ord)
instance IX.Ix Facing where
  range (m, n) = [m..n]
  inRange (m, n) i = m <= i && i <= n
  index b i | IX.inRange b i = IX.unsafeIndex b i
            | otherwise = error "Out of range indexing with Facing."
  unsafeIndex (low, _) i = fromEnum i - fromEnum low
data Face = TopFace | BottomFace | OutFace | InFace | LeftFace | RightFace
     | NoFace | Unknown deriving (Eq, Show, Enum, Bounded, Ord)
instance IX.Ix Face where
  range (m, n) = [m..n]
  inRange (m, n) i = m <= i && i <= n
  index b i | IX.inRange b i = IX.unsafeIndex b i
            | otherwise = error "Out of range indexing with Face."
  unsafeIndex (low, _) i = fromEnum i - fromEnum low

data MapContent = Open | Wall | Void deriving (Show, Eq)
data Action = Begin | Walk Int | Turn Direction deriving (Show, Eq)
data Direction = LeftD | RightD deriving (Show, Eq)

type MapLoc = (Int, Int)
type LocationState = (MapLoc, Facing)
type MapArr  = UA.Array MapLoc MapContent
type AdjLoc  = A.Array Facing LocationState
type MoveArr = A.Array MapLoc AdjLoc
type TileArr = UA.Array MapLoc Bool
type CenterAdjArr = UA.Array MapLoc Int
type ExpectedAdjFaces = UA.Array Facing Face
type FaceAndAdjacent = (Face, ExpectedAdjFaces)
type FaceArr = A.Array MapLoc FaceAndAdjacent
type FaceIndArr = A.Array Face MapLoc

--
-- Code for Puzzle 22.
--

puzzle_22 :: IO (Int, Int)
puzzle_22 = do

  let inputFile = "puzzle_22.inp"

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap lines (readFile inputFile)

  -- Generate an array representing the map, the width/height/depth of the cube, the start
  -- location, and the list of commands converted to Actions.
  
  let (mapArr, cubeSize, startPoint, commands) = createMapArrayAndFindCubeSize puzzInp

  -- Compute the answer for part 1. Compute a move array to overlay with the map that for each
  -- location indicates the resulting location when exiting this location going a specific
  -- direction. For part 1, this is wraps around the map as described, and is pre-computed before
  -- following the path, so the path following function can work the same for parts 1 and 2 and only
  -- the move array is different.

  let moveArr1 = genArrayMovesP1 mapArr
      initialState1 = (startPoint, East)
      ((finalX1, finalY1), finalDir1) = followPathToEnd mapArr moveArr1 initialState1 commands
      ansPart1 = 1000 * finalY1 + 4 * finalX1 + (facingValArr UA.! finalDir1)

  let moveArr2 = genArrayMovesP2 mapArr cubeSize
      initialState2 = (startPoint, East)
      ((finalX2, finalY2), finalDir2) = followPathToEnd mapArr moveArr2 initialState2 commands
      ansPart2 = 1000 * finalY2 + 4 * finalX2 + (facingValArr UA.! finalDir2)

  return (ansPart1, ansPart2)

-- Given a location in an array, determine the valid locations next to it, horizontally or
-- vertically, that are not out of bounds of the array. Return this list.

adjacentLocs :: UA.Array MapLoc a -> MapLoc -> [(MapLoc, Facing)]
adjacentLocs arr (x, y)
  = filter inBounds [((x, y - 1), North), ((x + 1, y), East), ((x, y + 1), South), ((x - 1, y), West)]
  where
    ((xLow, yLow), (xHigh, yHigh)) = UA.bounds arr

    inBounds :: (MapLoc, Facing) -> Bool
    inBounds ((x1, y1), _)
      | x1 < xLow || x1 > xHigh = False
      | y1 < yLow || y1 > yHigh = False
      | otherwise = True

-- Generate the array corresponding to the map array where each element and direction indicates what
-- the next location is with one step and the resulting direction. This function does that following
-- the rules of part 1.

genArrayMovesP1 :: MapArr -> MoveArr
genArrayMovesP1 mapArr = A.array bnds initList
  where
    initList = [((x, y), genMoves (x, y)) | x <- [xLow..xHigh], y <- [yLow..yHigh]]
    bnds@((xLow, yLow), (xHigh, yHigh)) = UA.bounds mapArr
    dirRange = [minBound..maxBound]

    -- Generate the single step moves for all four directions from this location and return them in
    -- an array.

    genMoves :: MapLoc -> AdjLoc
    genMoves loc = A.array (North, West) locInitList
      where
        locInitList = (zip dirRange . map (determineNextLoc loc)) dirRange

    -- Determine the next location including wrap-around. The location returned will be one inside
    -- the map and that location could contain a wall or be open.

    determineNextLoc :: MapLoc -> Facing -> LocationState
    determineNextLoc (x, y) dir = (nextLoc, dir)
      where

        -- The nextLoc is either the next step, or if it is out of range or is a void space, then
        -- the next open or wall space after wrapping around the map.

        nextLoc
          | outOfRange nextStep || mapArr UA.! nextStep == Void = wrappedLoc wrapStart incFn
          | otherwise = nextStep

        -- The nextStep is just the next location in line one step away.

        (nextStep, incFn, wrapStart)
          | dir == North = ((x, y - 1), second (subtract 1), (x, yHigh))
          | dir == East  = ((x + 1, y), first (+ 1), (xLow, y))
          | dir == South = ((x, y + 1), second (+ 1), (x, yLow))
          | otherwise    = ((x - 1, y), first (subtract 1), (xHigh, y))

    -- Find the wrapped location from here, using a start location and a function to increment the
    -- starting location to find a valid location.

    wrappedLoc :: MapLoc -> ((,) Int Int -> (,) Int Int) -> MapLoc
    wrappedLoc searchLoc incFn = (foldr findValidLoc (0, 0) . iterate incFn) searchLoc
      where
        findValidLoc currLoc nextLoc
          | contents == Open || contents == Wall = currLoc
          | otherwise = nextLoc
          where
            contents = mapArr UA.! currLoc

    -- Return true if this location is out of the array range.

    outOfRange :: MapLoc -> Bool
    outOfRange (x, y)
      | x < xLow || x > xHigh = True
      | y < yLow || y > yHigh = True
      | otherwise = False

-- Generate the array corresponding to the map array where each element and direction indicates what
-- the next location is with one step and the resulting direction. This function does that following
-- the rules of part 2.

genArrayMovesP2 :: MapArr -> Int -> MoveArr
genArrayMovesP2 mapArr cubeSize = A.array bnds initList
  where
    initList = [((x, y), genMoves (x, y)) | x <- [xLow..xHigh], y <- [yLow..yHigh]]
    bnds@((xLow, yLow), (xHigh, yHigh)) = UA.bounds mapArr
    dirRange = [minBound..maxBound]

    -- Generate the single step moves for all four directions from this location and return them in
    -- an array.

    genMoves :: MapLoc -> AdjLoc
    genMoves loc = A.array (North, West) locInitList
      where
        locInitList = (zip dirRange . map (determineNextLoc loc)) dirRange

    -- Determine the next location including wrap-around. The location returned will be one inside
    -- the map and that location could contain a wall or be open.

    determineNextLoc :: MapLoc -> Facing -> LocationState
    determineNextLoc currLoc@(x, y) dir = (nextLoc, nextDir)
      where

        -- The nextLoc is either the next step, or if it is out of range or is a void space, then
        -- the next open or wall space after wrapping around the map.

        (nextLoc, nextDir)
          | outOfRange nextStep || mapArr UA.! nextStep == Void
          = aroundEdge currLoc dir
          | otherwise = (nextStep, dir)

        -- The nextStep is just the next location in line one step away.

        nextStep
          | dir == North = (x, y - 1)
          | dir == East  = (x + 1, y)
          | dir == South = (x, y + 1)
          | otherwise    = (x - 1, y)

    -- From the given position and direction, determine the position and direction from moving one
    -- step forward, given that we are at an edge. The resulting location will be on the tile that
    -- would be around the corner. We need to take into account the location on the new edge, and
    -- since the edges are all layed out flat on the map, the direction we are traveling may be
    -- different.

    aroundEdge :: MapLoc -> Facing -> (MapLoc, Facing)
    aroundEdge currLoc dir = (newLoc, newDir)
      where
        newLoc = locFromRightOfTile nextFaceTileLoc reverseDir srcFromLeftOfTile
        srcFromLeftOfTile = fromLeftOfTile currLoc dir
        newDir = oppositeDir UA.! reverseDir
        reverseDir = (fst . head . filter ((== currFace) . snd) . UA.assocs) nextAdjFaces
        (_, nextAdjFaces) = faceArr A.! nextFaceTileLoc
        nextFaceTileLoc = faceIndArr A.! nextFace
        nextFace = currAdjFaces UA.! dir
        (currFace, currAdjFaces) = faceArr A.! tileLoc
        tileLoc = convertToTileLoc currLoc

    -- Given a tile location, a facing direction, and a number of steps from the right side of the
    -- tile, return the location on that edge of the map tile that many steps from the right facing
    -- out.

    locFromRightOfTile :: MapLoc -> Facing -> Int -> MapLoc
    locFromRightOfTile tileLoc facingDir srcStepsFromRight
      | facingDir == North = (nwX + stepsFromLeft, nwY)
      | facingDir == East  = (nwX + cubeSize - 1, nwY + stepsFromLeft)
      | facingDir == South = (nwX + srcStepsFromRight, nwY + cubeSize - 1)
      | otherwise          = (nwX, nwY + srcStepsFromRight)
      where
        (nwX, nwY) = convertToNWMapLoc tileLoc
        stepsFromLeft = cubeSize - srcStepsFromRight - 1

    -- Given a location in the tile array, a location in the map array, and a facing direction,
    -- return the number of elements from the left edge of the map tile the map location is when
    -- facing the facing direction.

    fromLeftOfTile :: MapLoc -> Facing -> Int
    fromLeftOfTile (x, y) facingDir
      | facingDir == North = compNorthEast x
      | facingDir == East  = compNorthEast y
      | facingDir == South = compSouthWest x
      | otherwise          = compSouthWest y
      where
        compNorthEast val = (val - 1) `rem` cubeSize
        compSouthWest val = cubeSize - ((val - 1) `rem` cubeSize) - 1

    -- Given a tile location, convert it to the location of the northwest corner of that tile in the map array.

    convertToNWMapLoc :: MapLoc -> MapLoc
    convertToNWMapLoc (tX, tY) = (convToMap tX, convToMap tY)
      where
        convToMap val = (val - 1) * cubeSize + 1

    -- Given a location on the full map, convert it to the location of the tile containing it in the
    -- tile array.

    convertToTileLoc :: MapLoc -> MapLoc
    convertToTileLoc (x, y) = ((x - 1) `quot` cubeSize + 1, (y - 1) `quot` cubeSize + 1)

    -- This array indicates which tile have data (True) and which are empty (False).

    tilePlaces :: TileArr
    tilePlaces = UA.array tileBounds
                 [((x, y), (mapArr UA.! (x * cubeSize, y * cubeSize)) /= Void)
                   | x <- [1..tileXHigh], y <- [1..tileYHigh]]

    -- This array holds integers corresponding to the times, and has higher numbers in the center of
    -- the array than at the edges. It is used to bias the selection of the top face to be closer to
    -- the center of the tiles.

    centerAdjArr :: CenterAdjArr
    centerAdjArr = UA.array tileBounds (map (addCenterAdj tileCenterInds) (UA.indices tilePlaces))
      where
        tileCenterInds = ((tileXHigh + 1) `quot` 2, (tileYHigh + 1) `quot` 2)

        -- Adjust for cnetrality of the tile location.

        addCenterAdj :: MapLoc -> MapLoc -> (MapLoc, Int)
        addCenterAdj (xCent, yCent) (iX, iY) = ((iX, iY), adjX + adjY)
          where
            adjX = compAdj iX xCent
            adjY = compAdj iY yCent
            compAdj ind cent = cent - abs (ind - cent)

    -- The score array for each tile taking into account how many tiles are adjacent to each one,
    -- and as a tie-break factor, a bonus for tile centrality.
    
    scoreArr :: CenterAdjArr
    scoreArr = UA.array tileBounds (map (genScore tilePlaces centerAdjArr) (UA.assocs tilePlaces))
      where
        factor = (xHigh - xLow) * (yHigh - yLow)

        -- Generate the score for the tile at the given location, given the number of valid tiles
        -- around it and the center scoring array added in.

        genScore :: TileArr -> CenterAdjArr -> (MapLoc, Bool) -> (MapLoc, Int)
        genScore tileArr centerAdjArr' (loc, sidePresent)
          | not sidePresent = (loc, 0)
          | otherwise = (loc, score)
          where
            score = (centerAdjArr' UA.! loc) + (factor * adjFullLocCount)
            adjFullLocCount = (length . filter id . map ((tileArr UA.!) . fst)) adjLocs
            adjLocs = adjacentLocs tileArr loc

    topLoc = (fst . maximumBy (compare `on` snd) . UA.assocs) scoreArr

    -- This is the tile-sized array indicating which tiles correspond with which faces of the cube,
    -- and for each one, the adjacent face expected on the drawn map off the four direction edges.
    -- Faces are determined working out from the initial face array that has only the top face
    -- defined.

    faceArr :: FaceArr
    faceArr = (fst . head . dropWhile (not . null . snd) . iterate determineAdjacentFaces)
              (faceInitArr, [topLoc])
    faceIndArr :: FaceIndArr
    faceIndArr = A.array (TopFace, RightFace) faceIndInitList
      where
        faceIndInitList = (map (\(loc, (face, _)) -> (face, loc)) . filter ((/= NoFace) . fst . snd)
                           . filter ((/= Unknown) . fst . snd) . A.assocs) faceArr

    -- This array will indicate the top face of the cube with all other faces unknown at this
    -- point. The top face is determined by finding the face with the most adjoining faces with
    -- ties being broken by closeness to the center.

    faceInitArr :: FaceArr
    faceInitArr = genInitialFaceArr tilePlaces topLoc

    -- The tile-sized arrays will have one element per cube face, so are much smaller. Compute the
    -- bounds here.
    
    (tileXHigh, tileYHigh) = (xHigh `quot` cubeSize, yHigh `quot` cubeSize)
    tileBounds = ((1, 1), (tileXHigh, tileYHigh))

    -- Return true if this location is out of the array range.

    outOfRange :: MapLoc -> Bool
    outOfRange (x, y)
      | x < xLow || x > xHigh = True
      | y < yLow || y > yHigh = True
      | otherwise = False

-- Generate a tile array with the top face defined by the given location, and the other locations
-- undefined.

genInitialFaceArr :: TileArr -> MapLoc -> FaceArr
genInitialFaceArr tileArr topLoc = A.array (UA.bounds tileArr) initList
  where
    initList = map initialState (UA.indices tileArr)
    initialState :: MapLoc -> (MapLoc, (Face, ExpectedAdjFaces))
    initialState loc
      | not $ tileArr UA.! loc = (loc, (NoFace, unknownNeighbors))
      | loc == topLoc = (loc, (TopFace, topFaceNeighbors))
      | otherwise = (loc, (Unknown, unknownNeighbors))
      where
        unknownNeighbors = UA.array (North, West) (zip [North .. West] (repeat Unknown))
        topFaceNeighbors = UA.array (North, West)
                                    (zip [North .. West] [OutFace, RightFace, InFace, LeftFace])

-- Given a face array and a list of tile locations in that array whose face was just determined,
-- find any adjacent tiles that we can now determine the face correspondence of as well as the
-- orientation, and return a new array with these fixed along with the locations of the newly fixed
-- faces.

determineAdjacentFaces :: (FaceArr, [MapLoc]) -> (FaceArr, [MapLoc])
determineAdjacentFaces (currFaceArr, newlyDefinedFaces) = result
  where
    result = foldl' determineSurroundingFaces (currFaceArr, []) newlyDefinedFaces

    determineSurroundingFaces :: (FaceArr, [MapLoc]) -> MapLoc -> (FaceArr, [MapLoc])
    determineSurroundingFaces (accFaceArr, accNextFaces) currLoc = (newFaceArr, newAccFaces)
      where
        newFaceArr = accFaceArr A.// assignedFaces
        (assignedFaces, newAccFaces) = foldl' assignFaceAndOrient ([], accNextFaces) adjLocsAndDir
        adjLocsAndDir = (filter ((== Unknown) . fst . (accFaceArr UA.!) . fst)
                         . adjacentLocs accFaceArr) currLoc
        (currFace, currAdjFaces) = accFaceArr A.! currLoc

        assignFaceAndOrient :: ([(MapLoc, FaceAndAdjacent)], [MapLoc]) -> (MapLoc, Facing)
                               -> ([(MapLoc, FaceAndAdjacent)], [MapLoc])
        assignFaceAndOrient (accAssigned, accNewFaces) (loc, dir)
          = (newAssigned : accAssigned, loc : accNewFaces)
          where
            newAssigned = (loc, (newFace, newExpectedArr))
            newFace = currAdjFaces UA.! dir
            newExpectedArr = genExpectedArr newFace dir currFace

    genExpectedArr :: Face -> Facing -> Face -> ExpectedAdjFaces
    genExpectedArr newFace dirFromOld oldFace = UA.array (North, West) newAdjFaces
      where
        newAdjFaces = (take 4 . dropWhile ((/= North) . fst)) (zip cycleOfDirs cycleOfFaces)
        cycleOfDirs = dropWhile (/= dirFromNewToOld) (cycle [North .. West])
        cycleOfFaces = dropWhile (/= oldFace) (cycle facesFromNew)
        dirFromNewToOld = oppositeDir UA.! dirFromOld
        facesFromNew = adjFacesArr A.! newFace

-- The value assigned to each facing direction.

facingValArr :: UA.Array Facing Int
facingValArr = UA.array (North, West) [(North, 3), (East, 0), (South, 1), (West, 2)]

-- Given a map array and a corresponding move array, a current location and facing direction, and a
-- list of actions, follow the list of actions to the end, then return the final location and
-- direction.

followPathToEnd :: MapArr -> MoveArr -> LocationState -> [Action] -> LocationState

-- If there are no more actions to perform, return the current location and direction.

followPathToEnd _ _ locOrient [] = locOrient

-- This should never happen, as we only use Begin to initiate creation of the list, and it never
-- ends up in the final list.

followPathToEnd _ _ _ (Begin : _) = error "Begin action found in command list."

-- If the current action is to turn, then adjust the location and direction and move on.

followPathToEnd mapArr moveArr (loc, orient) ((Turn dir) : cmds)
  = followPathToEnd mapArr moveArr (loc, newOrient) cmds
  where
    newOrient
      | orient == North = if dir == LeftD then West else East
      | orient == East  = if dir == LeftD then North else South
      | orient == South = if dir == LeftD then East else West
      | otherwise       = if dir == LeftD then South else North

-- Here we have a walk action. We handle these one step at a time and take into account each thing
-- that can happen for one step, then recursively call for the next step.

followPathToEnd mapArr moveArr locOrient@(loc, orient) (Walk steps : cmds)

  -- We are done walking, move on to next action.

  | steps == 0 = followPathToEnd mapArr moveArr locOrient cmds

  -- If we have run into a wall, either by stepping once or by wrapping around, we are done with
  -- this walk, and we go back to the prior location and move on with the next action.

  | proposedCont == Wall = followPathToEnd mapArr moveArr locOrient cmds

  -- If the next location, one step or wrapped is empty, then we keep going.

  | proposedCont == Open = followPathToEnd mapArr moveArr proposedLocDir (Walk nextSteps : cmds)

  -- This should never happen.

  | otherwise = error "Unexpected content in followPathToEnd."
  where
    nextSteps = steps - 1
    proposedCont = mapArr UA.! proposedLoc
    proposedLocDir@(proposedLoc, _) = (moveArr A.! loc) A.! orient

-- Define the adjacent faces clockwise for each face from the outside of the cube.

adjFacesArr :: A.Array Face [Face]
adjFacesArr = A.array (TopFace, Unknown) [(TopFace, [OutFace, RightFace, InFace, LeftFace]),
                                          (BottomFace, [OutFace, LeftFace, InFace, RightFace]),
                                          (OutFace, [BottomFace, RightFace, TopFace, LeftFace]),
                                          (InFace, [TopFace, RightFace, BottomFace, LeftFace]),
                                          (LeftFace, [OutFace, TopFace, InFace, BottomFace]),
                                          (RightFace, [OutFace, BottomFace, InFace, TopFace]),
                                          (NoFace, []),
                                          (Unknown, [])]

-- The opposite direction from the index direction.

oppositeDir :: UA.Array Facing Facing
oppositeDir = UA.array (North, West) [(North, South), (East, West), (South, North), (West, East)]

-- Take the puzzle input, and separate out the map from the path instructions.  Create a
-- two-dimensional array holding the map as drawn with each element a MapContent. Convert the path
-- instructions to a list of Actions. We find in part 2 that the reachable places on the map
-- represent the faces of a cube, so also compute the face width/height of the cube. Also, figure
-- out the start point for both parts, which is the top open map space moving left to right.
-- Tokenize the list of path instructions into a list of Actions.

createMapArrayAndFindCubeSize :: [[Char]] -> (MapArr, Int, MapLoc, [Action])
createMapArrayAndFindCubeSize puzzInp = (mapArr, cubeSize, startPoint, commands)
  where
    mapArr = convertInputLinesToArray maxMapWidth mapLinesUniform
    cubeSize = (isqrtG . (`quot` 6) . length . filter (/= Void) . UA.elems) mapArr
    startPoint = (fst . head . filter ((== Open) . snd)
                 . filter ((== 1) . snd . fst) . UA.assocs) mapArr
    commands = tokenize Begin pathDesc
    puzzInpNoEmpties = filter (not . null) puzzInp
    mapLineCount = length puzzInpNoEmpties - 1
    (mapDesc, pathDescList) = splitAt mapLineCount puzzInpNoEmpties
    pathDesc = head pathDescList
    maxMapWidth = (maximum . map length) mapDesc
    mapLinesUniform = map (take maxMapWidth . (++ repeat ' ')) mapDesc

-- This probably would have been simpler to write as a parser, which I have done in most of these
-- solutions, but I thought this would be interesting this time.

tokenize :: Action -> String -> [Action]

-- The Begin action is just a placeholder for the first call, and is never included in the result.

tokenize Begin [] = []
tokenize Begin xss@(x : xs)
  | x == 'R' = tokenize (Turn RightD) xs
  | x == 'L' = tokenize (Turn LeftD) xs
  | isDigit x = tokenize (Walk (read [x])) xs
  | otherwise = error ("Bad path description at: " ++ xss)

-- We are holding a turn action, and we see either another turn or a walk. Put the turn on the front
-- of the output, and call tokenize on XS for the rest, with the new current action being the
-- appropriate turn or walk.

tokenize action@(Turn _) [] = [action]
tokenize action@(Turn _) xss@(x : xs)
  | x == 'R' = action : tokenize (Turn RightD) xs
  | x == 'L' = action : tokenize (Turn LeftD) xs
  | isDigit x = action : tokenize (Walk (read [x])) xs
  | otherwise = error ("Bad path description at: " ++ xss)

-- Here we are holding a walk action, and if we see nothing more in the input or we see a turn, then
-- output it as is. If we see another digit, then add it to the current value multiplied by 10 then
-- pass it as the current token to the next call. It is for this situation that we need to have a
-- current token, since we may augment the current one if we see more digits.

tokenize action@(Walk _) [] = [action]
tokenize action@(Walk dist) xss@(x : xs)
  | x == 'R' = action : tokenize (Turn RightD) xs
  | x == 'L' = action : tokenize (Turn LeftD) xs
  | isDigit x = tokenize (Walk (dist * 10 + read [x])) xs
  | otherwise = error ("Bad path description at: " ++ xss)

-- Given the input lines, generate an array to represent the initial grove.

convertInputLinesToArray :: Int -> [String] -> UA.Array MapLoc MapContent
convertInputLinesToArray rowLen strs
  = UA.array ((1, 1), (rowLen, rowCount)) arrayInitializationList
  where
    arrayInitializationList = concat $ zipWith addInd convertedInput [1..]
    convertedInput = map (map convCharToEnum) strs
    rowCount = length strs

    addInd :: [MapContent] -> Int -> [(MapLoc, MapContent)]
    addInd rowContents yInd = zipWith (\xInd content -> ((xInd, yInd), content)) [1..] rowContents

convCharToEnum :: Char -> MapContent
convCharToEnum ch
  | ch == '.' = Open
  | ch == '#' = Wall
  | ch == ' ' = Void
  | otherwise = error ("Unexpected input character: " ++ show ch)
