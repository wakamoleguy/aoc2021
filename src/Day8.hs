module Day8 (part8a, part8b) where
import           Data.Foldable   (foldl')
import           Data.List       (sort)
import           Data.List.Split (splitOn)
import           Util            (readLines)

--------------------------------------------------------------------------------
-- Day 8 - Seven Segment Search
--------------------------------------------------------------------------------

input = map (map words . splitOn "|") <$> readLines "inputs/day8.txt"

countDigits1478 :: [String] -> Int
countDigits1478 = length . filter (flip elem [2, 3, 4, 7] . length)

-- Part 2
-- Position = encoded value:
--  0 = 6 letter group that has all 3 letters of 3 letter group, but not all 4 of 4 letter group
--  1 = 2 letter group
--  2 = 5 letter group that has segment that digit 8 has over digit 9 (segment e)
--  3 = 5 letter group that has all 3 letters of 3 letter group
--  4 = 4 letter group
--  5 = 5 letter group that...is not digit 2 or digit 3? - has segment that digit 3 has that digit 2 does not
--  6 = 6 letter group that does not have both letters of 2 letter group
--  7 = 3 letter group
--  8 = 7 letter group
--  9 = 6 letter group that has all 4 letters of 4 letter group

decodeDigits :: [String] -> (String, String, String, String, String, String, String, String, String, String)
decodeDigits digits =
  let zero = head $ filter (\x -> length x == 6 && all (`elem` x) seven && not (all (`elem` x) four)) digits
      one = head $ filter (\x -> length x == 2) digits
      two = head $ filter (\x -> length x == 5 && missing89Segment `elem` x) digits
      three = head $ filter (\x -> length x == 5 && all (`elem` x) seven) digits
      four = head $ filter (\x -> length x == 4) digits
      five = head $ filter (\x -> length x == 5 && x /= two && x /= three) digits
      six = head $ filter (\x -> length x == 6 && not (all (`elem` x) one)) digits
      seven = head $ filter (\x -> length x == 3) digits
      eight = head $ filter (\x -> length x == 7) digits
      nine = head $ filter (\x -> length x == 6 && all (`elem` x) four) digits
      missing89Segment = head $ filter (`notElem` nine) eight
   in (sort zero, sort one, sort two, sort three, sort four, sort five, sort six, sort seven, sort eight, sort nine)

decodeOutput :: (String, String, String, String, String, String, String, String, String, String) -> String -> Int
decodeOutput (a, b, c, d, e, f, g, h, i, j) x
  | a == sort x = 0
  | b == sort x = 1
  | c == sort x = 2
  | d == sort x = 3
  | e == sort x = 4
  | f == sort x = 5
  | g == sort x = 6
  | h == sort x = 7
  | i == sort x = 8
  | j == sort x = 9
  | otherwise = error ("Invalid digit" ++ show x ++ " " ++ show (a, b, c, d, e, f, g, h, i, j))

decode :: [String] -> [String] -> Int
decode digits = foldl' (\b a -> b * 10 + decode' a) 0
  where
    decode' = decodeOutput (decodeDigits digits)

part8a = sum . map countDigits1478 . concatMap tail <$> input
part8b = sum . map (\line -> decode (head line) (head (tail line))) <$> input
