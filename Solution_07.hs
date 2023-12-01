-- For Advent of Code 2022
--
-- By Truman Collins
-- December 7, 2022


module Solution_07 (
  puzzle_07
) where

import Data.List
import Data.Maybe
import Data.Function
import Control.Applicative
import Control.Monad
import Parsers


--
-- Code for Puzzle 7.
--

-- The various lines of input.

data ScreenLine = CommandCD String | CommandLS | LSResDir String | LSResFile Int String deriving Show

-- Returns true if this line is output from an 'ls' command.

isLSOutput :: ScreenLine -> Bool
isLSOutput (LSResDir _) = True
isLSOutput (LSResFile _ _) = True
isLSOutput _ = False

-- This data structure holds either a directory or file. A more elaborate data structure should be
-- used in the real world.

data DirOrFile = DirOrFile { _name   :: String
                           , _isFile :: Bool
                           , _size   :: Int
                           , _files  :: [DirOrFile]
                           , _dirs   :: [DirOrFile]
                           } deriving Show

puzzle_07 :: IO (Int, Int)
puzzle_07 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile "puzzle_07.inp")

  when (badParseOfInputLines puzzInp)
       (ioError $ userError "Bad input for puzzle 7.")

  let inputData = map (fst . head) puzzInp
      initialEmptyDir = DirOrFile "/" False 0 [] []

      -- Construct the directory tree, then populate it with sizes.

      dirTree = consumeInputLines [initialEmptyDir] inputData
      dirTreeWithSizes = computeSizes dirTree

      -- Compute the answer to part 1.

      sizesLELimit = collectSizesLELimit 100000 [] dirTreeWithSizes
      part1Ans = sum sizesLELimit

      -- Compute the answer to part 2.

      totalDirSize = _size dirTreeWithSizes
      spaceToFree  = 30000000 - (70000000 - totalDirSize)
      part2Ans     = findMinDirSize spaceToFree totalDirSize dirTreeWithSizes

  return (part1Ans, part2Ans)

-- Given an 'ls' command read all of the following lines of output until the next command, and add
-- them to a new directory entry with the given name. Keep the files and directories in separate
-- lists, and sort each by name.

handleLS :: String -> [ScreenLine] -> (DirOrFile, [ScreenLine])
handleLS name [] = (DirOrFile name False 0 [] [], [])
handleLS name screenLines = (DirOrFile name False 0 filesS dirsS, remainingScreenLines)
  where
    [filesS, dirsS] = map (sortBy (compare `on` _name)) [files, dirs]
    (files, dirs) = foldr readFilesAndDirs ([], []) lsOutput
    (lsOutput, remainingScreenLines) = span isLSOutput screenLines

    readFilesAndDirs :: ScreenLine -> ([DirOrFile], [DirOrFile]) -> ([DirOrFile], [DirOrFile])
    readFilesAndDirs (LSResDir dirName) (fileAcc, dirAcc)
      = (fileAcc, DirOrFile dirName False 0 [] [] : dirAcc)
    readFilesAndDirs (LSResFile siz fileName) (fileAcc, dirAcc)
      = (DirOrFile fileName True siz [] [] : fileAcc, dirAcc)
    readFilesAndDirs _ accs = accs

-- Go through the input lines constructing the directory tree as we go. I make the assumption here
-- that an 'ls' command is done in a directory before moving into a sub-directory within it.

consumeInputLines :: [DirOrFile] -> [ScreenLine] -> DirOrFile
consumeInputLines [] _ = error "No current directory."
consumeInputLines nestedDirectories [] = popAndIncorporateToTop nestedDirectories
consumeInputLines nestedDirectories@(currDir : upperDirs) ((CommandCD cdStr) : remainder)
  = case cdStr of
      "/"  -> consumeInputLines [popAndIncorporateToTop nestedDirectories] remainder
      ".." -> consumeInputLines (incorporateUp currDir upperDirs) remainder
      _    -> let currDirM = find ((== cdStr) . _name) (_dirs currDir)
              in  if isNothing currDirM then error ("Can't find directory " ++ cdStr)
                  else consumeInputLines (fromJust currDirM : nestedDirectories) remainder
consumeInputLines (currDir : upperDirs) (CommandLS : remainder)
  = let (newDir, otherCmds) = handleLS (_name currDir) remainder
    in  consumeInputLines (newDir : upperDirs) otherCmds
consumeInputLines _ _ = error "Unknown command."

-- For the stack of directories, pop each one off, and insert it in the directory list above in the
-- directory directly above it.

popAndIncorporateToTop :: [DirOrFile] -> DirOrFile
popAndIncorporateToTop [] = error "No directories to pop."
popAndIncorporateToTop [topDir] = topDir
popAndIncorporateToTop (currDir : upperDirs) = popAndIncorporateToTop newUpperDirs
  where
    newUpperDirs = incorporateUp currDir upperDirs

-- Insert the given directory into the directory list, in sorted order, of the first directory on
-- the list.

incorporateUp :: DirOrFile -> [DirOrFile] -> [DirOrFile]
incorporateUp _ [] = error "Can't incorporate into null directory."
incorporateUp currDir (nextUpDir : fartherUpDirs) = newNextUpDir : fartherUpDirs
  where
    newNextUpDir = DirOrFile (_name nextUpDir) False (_size nextUpDir) (_files nextUpDir) newDirs
    newDirs = replaceOrInsertCurrDir dirs
    dirs = _dirs nextUpDir

    -- Replace or insert the directory in to the list of directories sorted by name.

    replaceOrInsertCurrDir :: [DirOrFile] -> [DirOrFile]
    replaceOrInsertCurrDir [] = [currDir]
    replaceOrInsertCurrDir (d : ds)
      | ordRelation == EQ = currDir : ds
      | ordRelation == LT = currDir : d : ds
      | otherwise = d : replaceOrInsertCurrDir ds
      where ordRelation = compare (_name currDir) (_name d)

-- Recursively traverse the directory structure and compute the size of each directory.

computeSizes :: DirOrFile -> DirOrFile
computeSizes currDirOrFile@(DirOrFile dirName isFile _ dirFiles dirDirs)

  -- Here if we have a file, just return it because it was created with its size.

  | isFile = currDirOrFile

  -- In any other case, recursively compute the sizes of any sub-directories, then add these sizes
  -- to the sizes of the files to get the total size of this directory.

  | otherwise = DirOrFile dirName False newSize dirFiles newDirs
  where
    newSize = (sum . map _size) (newDirs ++ dirFiles)
    newDirs = map computeSizes dirDirs

-- Traverse the directory structure and collect all of the sizes less than or equal to the limit in
-- a list.

collectSizesLELimit :: Int -> [Int] -> DirOrFile -> [Int]
collectSizesLELimit _ acc (DirOrFile _ True _ _ _) = acc
collectSizesLELimit limit acc (DirOrFile _ _ currSize _ currDirs)
  | currSize <= limit = currSize : recAcc
  | otherwise = recAcc
  where
    recAcc = foldl' (collectSizesLELimit limit) acc currDirs

-- Find the minimum directory size over a given limit.

findMinDirSize :: Int -> Int -> DirOrFile -> Int
findMinDirSize spaceNeeded minSoFar dir = accBestOne minFromDeeper (_size dir)
  where
    minFromDeeper = foldl' (findMinDirSize spaceNeeded) minSoFar (_dirs dir)
    accBestOne acc x
      | x < acc && x >= spaceNeeded = x
      | otherwise = acc

-- Parse an input line.

readInputLine :: Parser ScreenLine
readInputLine = do
    _ <- symbol "$ cd .."
    return (CommandCD "..")
  <|> do
    _ <- symbol "$ cd /"
    return (CommandCD "/")
  <|> do
    _ <- symbol "$ cd"
    _ <- space
    CommandCD <$> identWithPeriod
  <|> do
    _ <- symbol "$ ls"
    return CommandLS
  <|> do
    _ <- symbol "dir"
    _ <- space
    LSResDir <$> identWithPeriod
  <|> do
    sz <- nat
    _ <- space
    LSResFile sz <$> identWithPeriod
