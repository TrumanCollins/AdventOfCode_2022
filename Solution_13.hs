-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_13 (
  puzzle_13
) where

import Data.List
import Data.Maybe
import Parsers
import Control.Applicative
import Control.Monad

data PacketElem = PacketTerm Int | PacketList [PacketElem] deriving (Show, Eq)
type PacketPair = (PacketElem, PacketElem)

--
-- Code for Puzzle 13.
--

puzzle_13 :: IO (Int, Int)
puzzle_13 = do

  -- Read in the file and convert it to a list of strings, one per line in the file.

  puzzInp <- fmap (parse readPacketPairs) (readFile "puzzle_13.inp")

  -- Make sure there was a valid parse of the input.

  when (badParseOfInput puzzInp)
       (ioError $ userError "Invalid input data for puzzle 13.")

  -- Compare the pairs and sum the indices of those in the right order.

  let packetPairList = (fst . head) puzzInp
      part1Ans = (sum . map fst . filter ((/= GT) . snd) . zip [1..] . map compareOrder)
                 packetPairList

  -- Put all of the packets in one list along with the two dividers, then sort the list and find the
  -- locations of the dividers.

  let individPacketList = foldr (\(p1, p2) acc -> p1 : p2 : acc) [] packetPairList
      dividers@[divider2, divider6] = map genDivider [2, 6]
      sortedPktsWithDividers = sortBy (curry compareOrder) (dividers ++ individPacketList)
      locDivider2 = fromJust $ elemIndex divider2 sortedPktsWithDividers
      locDivider6 = fromJust $ elemIndex divider6 sortedPktsWithDividers
      part2Ans = (locDivider2 + 1) * (locDivider6 + 1)

  return (part1Ans, part2Ans)

-- Build a divider element.

genDivider :: Int -> PacketElem
genDivider value = PacketList [PacketList [PacketTerm value]]

-- Return the comparison value between the two packets (LT, EQ, GT).

compareOrder :: PacketPair -> Ordering
compareOrder (PacketTerm val1, PacketTerm val2) = compare val1 val2
compareOrder (PacketList [], PacketList _) = LT
compareOrder (PacketList _, PacketList []) = GT
compareOrder (PacketList (x : xs), PacketList (y : ys))
  | firstCompare == EQ = compareOrder (PacketList xs, PacketList ys)
  | otherwise = firstCompare
  where
    firstCompare = compareOrder (x, y)
compareOrder (PacketTerm val1, pktList2) = compareOrder (PacketList [PacketTerm val1], pktList2)
compareOrder (pktList1, PacketTerm val2) = compareOrder (pktList1, PacketList [PacketTerm val2])

-- Parse the pairs of packets.

readPacketPairs :: Parser [PacketPair]
readPacketPairs = some readPacketPair

-- Parse a single packet pair.

readPacketPair :: Parser PacketPair
readPacketPair = do
  packet1 <- readListOfElems
  _ <- space
  packet2 <- readListOfElems
  return (packet1, packet2)

-- Read a list of elements. We handle an empty list separately.

readListOfElems :: Parser PacketElem
readListOfElems = do
    _ <- symbol "[]"
    return (PacketList [])
  <|> do
    _ <- symbol "["
    firstElem <- readListOrInt
    restOfElems <- many readCommaListOrInt
    _ <- symbol "]"
    return (PacketList (firstElem : restOfElems))

-- Read a list or an int.

readListOrInt :: Parser PacketElem
readListOrInt = do
    PacketTerm <$> int
  <|> readListOfElems

-- Read a comma and a list or int.

readCommaListOrInt :: Parser PacketElem
readCommaListOrInt = do
  _ <- symbol ","
  readListOrInt
