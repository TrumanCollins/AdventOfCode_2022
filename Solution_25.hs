-- For Advent of Code 2022
--
-- By Truman Collins
-- 2023

module Solution_25 (
  puzzle_25
) where

import Data.List
import Data.Function


--
-- Code for Puzzle 25.
--

puzzle_25 :: IO (String, Int)
puzzle_25 = do

  let (inputFile, _puzzleNumber) = ("puzzle_25.inp", 25 :: Int)

  -- Read in the file and convert it to a list of strings, one per line in the file.

  snafuFuelRequirements <- fmap lines (readFile inputFile)

  let ansPart1 = (base10ToSnafu . sum . map snafuToBase10) snafuFuelRequirements

  return (ansPart1, 0)


-- Converting to base 10 is easy, just walking through the list adding the "digit" into to an
-- accumulator that is multiplied by 5 each iteration.

snafuToBase10 :: String -> Integer
snafuToBase10 = go 0
  where
    go :: Integer -> String -> Integer
    go acc [] = acc
    go acc (c : cs) = go newAcc cs
      where
        newAcc = (acc * 5) + case c of
                               '2' ->  2
                               '1' ->  1
                               '0' ->  0
                               '-' -> -1
                               '=' -> -2
                               _   -> error "Invalid snafu digit."

-- Convert a base-10 integer to Snafu.

base10ToSnafu :: Integer -> String
base10ToSnafu 0 = "0"
base10ToSnafu value = reverse revSnafuDigits
  where
    (revSnafuDigits, _) = foldl' findNextChar ([firstDigit], totalLeft) ranges2
    (firstDigit, totalLeft, expon) = find1stDigAndRemainder value ranges
    ranges2 = (reverse . take expon . iterate (map (* 5))) [2, 1, 0, -1, -2]
    ranges = iterate (\(x, y, z, e) -> (x * 5, y * 5 + 2, z * 5 + 2, e + 1)) (1, 1, 2, 0)

    -- Given a value (positive or negative), look at the 5 options for digits and choose the one
    -- that will result in the smallest absolute value remaining.

    findNextChar :: (String, Integer) -> [Integer] -> (String, Integer)
    findNextChar (accSnafu, totRemain) vals = (ch : accSnafu, newRemain)
      where
        (ch, newRemain) = (minimumBy (compare `on` (abs . snd))
                           . zip ['2','1','0','-','='] . map (totRemain -)) vals

    -- Find the first digit of the Snafu number, which will be 1 or 2.

    find1stDigAndRemainder :: Integer -> [(Integer, Integer, Integer, Int)] -> (Char, Integer, Int)
    find1stDigAndRemainder _ [] = ('0', 0, 0)
    find1stDigAndRemainder val ((x, y, z, e) : xs)
      | val > z = find1stDigAndRemainder val xs
      | val <= y = ('1', val - x, e)
      | otherwise = ('2', val - (2 * x), e)
