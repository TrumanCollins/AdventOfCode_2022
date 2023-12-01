-- For Advent of Code 2022
--
-- By Truman Collins
-- December 5, 2022

module Solution_05 (
  puzzle_05
) where

import Data.List
import Data.Maybe
import qualified Data.Array as A
import Control.Applicative
import Control.Monad
import Parsers


--
-- Code for Puzzle 5.
--

type Stack = [Char]
type StackArr = A.Array Int Stack
type Moves = [Move]
data Move = Move { _fromStack :: Int
                 , _toStack   :: Int
                 , _count     :: Int
                 } deriving Show
data StacksAndMoves = StacksAndMoves { _stacksArr :: StackArr
                                     , _moves     :: Moves
                                     } deriving Show

-- Parsing the input for this one was a little challenging because whitespace is important and most
-- of the parsing functions intentionally ignore whitespace.

readInput :: Parser StacksAndMoves
readInput = do
  stackRows <- readInitialStacks
  colCount  <- readColumnNumbers
  let stackRowsSameLen = map (extendTo colCount) stackRows
      stackCols = (map (map fromJust . dropWhile isNothing) . transpose) stackRowsSameLen
      stackArray = A.array (1, colCount) (zip [1..] stackCols)
  StacksAndMoves stackArray <$> readMoves
  where
    extendTo :: Int -> [Maybe Char] -> [Maybe Char]
    extendTo count currList = let currLen = length currList
                              in  currList ++ replicate (count - currLen) Nothing

readInitialStacks :: Parser [[Maybe Char]]
readInitialStacks = many readStackRow

readStackRow :: Parser [Maybe Char]
readStackRow = do
                 row <- many readStackElementNotEnd
                 lastOne <- readStackElementEnd
                 _ <- newline
                 return (row ++ [lastOne])

-- Here I use sat rather than symbol with a string because symbol ignores whitespace.

readStackElementNotEnd :: Parser (Maybe Char)
readStackElementNotEnd = do
                     _ <- sat (== ' ')
                     _ <- sat (== ' ')
                     _ <- sat (== ' ')
                     _ <- sat (== ' ')
                     return Nothing
                   <|> do
                     _ <- symbol "["
                     ch <- upper
                     _ <- sat (== ']')
                     _ <- sat (== ' ')
                     return (Just ch)

readStackElementEnd :: Parser (Maybe Char)
readStackElementEnd = do
                     _ <- symbol "["
                     ch <- upper
                     _ <- sat (== ']')
                     return (Just ch)

readColumnNumbers :: Parser Int
readColumnNumbers = do
  colNumbers <- some readColNumber
  _ <- space
  return (length colNumbers)

readColNumber :: Parser Int
readColNumber = do
  _ <- space
  int

readMoves :: Parser Moves
readMoves = some readMove

readMove :: Parser Move
readMove = do
  _ <- symbol "move"
  _ <- spaceNotNL
  count <- int
  _ <- spaceNotNL
  _ <- symbol "from"
  _ <- spaceNotNL
  from <- int
  _ <- spaceNotNL
  _ <- symbol "to"
  _ <- spaceNotNL
  to <- int
  _ <- space
  return (Move from to count)

-- Make a move on the stack array returning the resulting stack array. The boolean value passed in
-- is used to indicate whether to move the stacks one box at a time or as one unit.

makeStackMove :: Bool -> StackArr -> Move -> StackArr
makeStackMove oneAtATime currArr (Move fromStack toStack count)
  = currArr A.// [(fromStack, remainder), (toStack, newToStack)]
  where
    newToStack
      | oneAtATime = foldl' (flip (:)) (currArr A.! toStack) toTransfer
      | otherwise = toTransfer ++ (currArr A.! toStack)
    (toTransfer, remainder) = splitAt count (currArr A.! fromStack)

getTopLetters :: StackArr -> String
getTopLetters = map head . A.elems

puzzle_05 :: IO (String, String)
puzzle_05 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  parseResult <- fmap (parse readInput) (readFile "puzzle_05.inp")

  -- Make sure there was a valid parse of the input.

  when (badParseOfInput parseResult)
       (ioError $ userError "Invalid input data for puzzle 5.")

  let (StacksAndMoves stackArr moves) = (fst . head) parseResult
      finalStackArr1 = foldl' (makeStackMove True) stackArr moves
      finalTops1 = getTopLetters finalStackArr1
      finalStackArr2 = foldl' (makeStackMove False) stackArr moves
      finalTops2 = getTopLetters finalStackArr2

  return (finalTops1, finalTops2)
