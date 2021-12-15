module Day15 (part15a, part15b) where
import           Data.Array       (Array, bounds, listArray, (!))
import           Data.Foldable    (toList)
import           Data.Graph.AStar (aStar)
import qualified Data.HashSet     as HS
import qualified Data.Map.Lazy    as Map
import qualified Data.Set         as Set
import           Util             (readLines)

--------------------------------------------------------------------------------
-- Day 15 - Chitons
--------------------------------------------------------------------------------

type Grid = Array Int (Array Int Int)

readGrid :: IO (Array Int (Array Int Int))
readGrid = do
  lines <- readLines "inputs/day15.txt"
  let width = length lines
  return $ listArray (0, width - 1) $ map (listArray (0, width - 1) . map (read . (:[]))) lines

readExample :: IO (Array Int (Array Int Int))
readExample = do
  lines <- readLines "inputs/day15-example.txt"
  let width = length lines
  return $ listArray (0, width - 1) $ map (listArray (0, width - 1) . map (read . (:[]))) lines

riskValue grid (i, j) =
      let
        width = snd (bounds grid) + 1
        raw = (grid ! (i `mod` width) ! (j `mod` width)) + (i `div` width) + (j `div` width)
      in if raw >= 10 then raw - 9 else raw

-- memoRiskPath grid coord = memoRiskPath' coord
--   where
--     width = snd (bounds grid) + 1
--     riskValue (i, j) =
--       let raw = (grid ! (i `mod` width) ! (j `mod` width)) + (i `div` width) + (j `div` width)
--       in if raw >= 10 then raw - 9 else raw
--     memoRiskPath' coord = Map.findWithDefault 10000 coord riskPathMap
--     riskPathMap = Map.fromList [ ((i, j), riskPath' (i, j)) | i <- [0 .. fst coord], j <- [0 .. snd coord] ]
--     riskPath' _ (0, 0) = 0
--     riskPath' _ (0, j) = riskValue (0, j) + memoRiskPath' (0, j-1)
--     riskPath' _ (i, 0) = riskValue (i, 0) + memoRiskPath' (i-1, 0)
--     riskPath' (fi, fj) (i, j) = riskValue (i, j) + minimum [memoRiskPath' (i, j) (i-1, j), memoRiskPath' (i, j-1), memoRiskPath' (i+1, j), memoRiskPath' (i, j+1)]

findPath :: Grid -> (Int, Int) -> Int
findPath grid start =
  let
    neighbors (i, j) = HS.fromList $ [ (x, y) | (x, y) <- [(i-1, j), (i, j-1), (i+1, j), (i, j+1)], x>=0, y>=0, x <= fst start, y <= snd start]
    weight (i, j) (x, y) = riskValue grid (i, j) + riskValue grid (x, y)
    heur (i, j) = i + j
    goal (0, 0) = True
    goal _      = False
    path = aStar neighbors weight heur goal start
    pathSum = sum $ maybe [] (map (riskValue grid)) path
  in riskValue grid start - riskValue grid (0, 0) + pathSum

part15a = do
  grid <- readGrid
  let width = snd (bounds grid)
  return $ findPath grid (width, width)
part15b = do
  grid <- readGrid
  let width = (snd (bounds grid) + 1) * 5
  return $ findPath grid (width - 1, width - 1)
