module Day15 (part15a, part15b) where
import           Data.Array       (Array, bounds, listArray, (!))
import           Data.Foldable    (toList)
import           Data.Graph.AStar (aStar)
import qualified Data.HashSet     as HS
import           Data.List.Split  (chunksOf)
import qualified Data.Map.Lazy    as Map
import qualified Data.Set         as Set
import           Util             (readLines)

--------------------------------------------------------------------------------
-- Day 15 - Chitons
--------------------------------------------------------------------------------

type Grid = Array Int (Array Int Int)

input :: IO [String]
input = readLines "inputs/day15.txt"
example :: IO [String]
example = readLines "inputs/day15-example.txt"

readDigits :: String -> [Int]
readDigits = map read . chunksOf 1

listToArray :: [a] -> Array Int a
listToArray a = listArray (0, length a - 1) a

readGrid :: [String] -> Grid
readGrid = listToArray . map (listToArray . readDigits)

riskValue :: Grid -> (Int, Int) -> Int
riskValue grid (i, j) =
      let
        width = snd (bounds grid) + 1
        raw = (grid ! (i `mod` width) ! (j `mod` width)) + (i `div` width) + (j `div` width)
      in (raw `mod` 10) + (raw `div` 10)

findPath :: Grid -> (Int, Int) -> Int
findPath grid start =
  let
    neighbors (i, j) = HS.fromList $ [ (x, y) | (x, y) <- [(i-1, j), (i, j-1), (i+1, j), (i, j+1)], x>=0, y>=0, x <= fst start, y <= snd start]
    weight _ = riskValue grid
    heur (i, j) = i + j
    goal (0, 0) = True
    goal _      = False
    path = aStar neighbors weight heur goal start
    pathSum = sum $ maybe [] (map (riskValue grid)) path
  in riskValue grid start - riskValue grid (0, 0) + pathSum

part15a :: IO Int
part15a = do
  grid <- readGrid <$> input
  let width = snd (bounds grid) + 1
  return $ findPath grid (width-1, width-1)
part15b :: IO Int
part15b = do
  grid <- readGrid <$> input
  let width = (snd (bounds grid) + 1) * 5
  return $ findPath grid (width-1, width-1)
