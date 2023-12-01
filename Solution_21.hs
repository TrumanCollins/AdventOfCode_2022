-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_21 (
  puzzle_21
) where

import Data.Maybe
import Data.Int
import Data.Ratio
import qualified Data.Map as M
import qualified Data.Bifunctor as BF
import Control.Applicative
import Control.Monad
import Parsers

data Operation = Plus | Minus | Mult | Divide deriving (Show, Eq)
data Expression = Term Rational | Oper Operation String String deriving Show

type MonkeyJobMap = M.Map String Rational

--
-- Code for Puzzle 21.
--

-- The idea here is that each expression (term or operation) is stored in a map keyed off the
-- monkey's name. The value in the map is the evaluated expression, but due to lazy evaluation,
-- isn't actually evaluated until needed. The actual evaluation takes in the map itself, and so is
-- able to reference other values.

puzzle_21 :: IO (Int64, Int64)
puzzle_21 = do

  let (topLevelMonkey, inputFile) = ("root", "puzzle_21.inp")

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (map (parse readInputLine) . lines) (readFile inputFile)

  -- Make sure the input was parsed correctly and retrive it out of the returned parser data
  -- structure.

  when (badParseOfInputLines puzzInp)
       (ioError $ userError ("Bad input for puzzle 21 from file " ++ inputFile))

  let monkeyJobs = map (fst . head) puzzInp
      monkeyJobMap = M.fromList (map (BF.second (evaluate monkeyJobMap)) monkeyJobs)
      ansPart1R  = fromJust $ M.lookup topLevelMonkey monkeyJobMap
      ansPart1   = fromIntegral (numerator ansPart1R `div` denominator ansPart1R)

  let lowVal = 10000000000000
      lowResult = genValueWithHm monkeyJobs lowVal
      highVal = 0
      highResult = genValueWithHm monkeyJobs highVal
      ansPart2 = binarySearch monkeyJobs (lowVal, lowResult) (highVal, highResult)

  return (ansPart1, ansPart2)

-- Do a binary search for the resulting value of zero.

binarySearch :: [(String, Expression)] -> (Int64, Rational) -> (Int64, Rational) -> Int64
binarySearch monkJobsOrig (lowVal, lowResult) (highVal, highResult)
  | midResult == 0 % 1 = midVal
  | midResult < 0 % 1  = binarySearch monkJobsOrig (midVal, midResult) (highVal, highResult)
  | otherwise = binarySearch monkJobsOrig (lowVal, lowResult) (midVal, midResult)
  where
    midVal = (lowVal + highVal) `quot` 2
    midResult = genValueWithHm monkJobsOrig midVal

-- Given a value for the "humn" one, incorporate it in the list of (name, expression) pairs and
-- solve for "root".

genValueWithHm :: [(String, Expression)] -> Int64 -> Rational
genValueWithHm monkJobsOrig newHmVal = ansR
  where
    ansR = fromJust $ M.lookup "root" monkeyJobMap
    monkeyJobMap = M.fromList (map (BF.second (evaluate monkeyJobMap)) monkeyJobs)
    monkeyJobs   = substHumnRoot monkJobsOrig

    substHumnRoot :: [(String, Expression)] -> [(String, Expression)]
    substHumnRoot [] = []
    substHumnRoot (x@(str, expr) : xs)
      | str == "root" = let (exprN1, exprN2) = getExprs expr
                        in  ("root", Oper Minus exprN1 exprN2) : processedRest
      | str == "humn" = ("humn", Term (fromIntegral newHmVal % 1)) : processedRest
      | otherwise = x : processedRest
      where
        processedRest = substHumnRoot xs

        getExprs :: Expression -> (String, String)
        getExprs (Oper _ exprN1 exprN2) = (exprN1, exprN2)
        getExprs (Term _) = error "Term found when Oper expected."

-- Evaluate the expression. If it is an operator with two operands, held as strings, look them up in
-- the monkey job map, which will cause recursive evaluation. Note that we assume that all names
-- used will be in the map, which we wouldn't normally do.

evaluate :: MonkeyJobMap -> Expression -> Rational
evaluate _ (Term x) = x
evaluate mjMap (Oper op exprName1 exprName2) = result
  where
    result = case op of
      Plus   -> expr1 + expr2
      Minus  -> expr1 - expr2
      Mult   -> expr1 * expr2
      Divide -> expr1 / expr2
    expr1 = (fromJust . M.lookup exprName1) mjMap
    expr2 = (fromJust . M.lookup exprName2) mjMap

readInputLine :: Parser (String, Expression)
readInputLine = do
  name <- identAlpha
  _ <- symbol ":"
  _ <- space
  readExpr name

readExpr :: String -> Parser (String, Expression)
readExpr name = do
    x <- int
    _ <- space
    return (name, Term (fromIntegral x % 1))
  <|> do
    exprName1 <- identAlpha
    _ <- space
    op <- readOp
    _ <- space
    exprName2 <- identAlpha
    _ <- space
    return (name, Oper op exprName1 exprName2)

readOp :: Parser Operation
readOp = do
    _ <- symbol "+"
    return Plus
  <|> do
    _ <- symbol "-"
    return Minus
  <|> do
    _ <- symbol "*"
    return Mult
  <|> do
    _ <- symbol "/"
    return Divide
