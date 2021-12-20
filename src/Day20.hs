module Day20 (part20a, part20b) where
import           Data.Array    (Array, bounds, elems, inRange, (!))
import           Data.Foldable (toList)
import           Data.String   (String)
import           Debug.Trace
import           Util          (listToArray, readLines)

--------------------------------------------------------------------------------
-- Day 20 - Trench Map
--------------------------------------------------------------------------------

type Rules = Array Int Bool
type Grid = Array Int (Array Int Bool)

input :: IO (Rules, Grid)
input = do
  ls <- readLines "inputs/day20.txt"
  let rule = listToArray $ map parseBit $ head ls
  let grid = listToArray $ map (listToArray . map parseBit) (tail $ tail ls)
  return (rule, grid)

parseBit :: Char -> Bool
parseBit '#' = True
parseBit _   = False

countBits :: Array Int (Array Int Bool) -> Int
countBits = sum . fmap (sum . fmap (\x -> if x then 1 else 0))

bitStringToInt :: [Bool] -> Int
bitStringToInt = foldl (\acc x -> acc * 2 + (if x then 1 else 0)) 0

value :: Bool -> Grid -> Int -> Int -> Bool
value def grid r c = if inRange (bounds grid) r && inRange (bounds (grid ! r)) c then grid ! r ! c else def

stepCell :: Rules -> Bool -> Grid -> Int -> Int -> Bool
stepCell rules def grid r c =
  let neighbors = bitStringToInt [ value def grid y x | y <- [r - 1 .. r + 1], x <- [c - 1 .. c + 1]]
  in rules ! neighbors

step :: Rules -> Bool -> Grid -> Grid
step rules def grid =
  let
    ((minr, maxr), (minc, maxc)) = (bounds grid, bounds (grid ! 0))
    stepCell' = stepCell rules def grid
    stepped = listToArray [listToArray [
      stepCell' r c
      | c <- [minc - 1 .. maxc + 1]] | r <- [minr - 1 .. maxr + 1]]
  in stepped

stepN :: Int -> Rules -> Grid -> Grid
stepN 0 r g = g
stepN i r g = stepN (i - 1) r $ step r (i `mod` 2 == 1) g

part20a :: IO Int
part20a = do
  (rules, grid) <- input
  return $ countBits $ stepN 2 rules grid

part20b :: IO Int
part20b = do
  (rules, grid) <- input
  return $ countBits $ stepN 50 rules grid
