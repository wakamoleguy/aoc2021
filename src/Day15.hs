module Day15 (part15a, part15b) where
import           Data.Array       (Array, Ix (inRange), bounds, listArray, (!))
import           Data.Foldable    (toList)
import           Data.Graph.AStar (aStar)
import qualified Data.HashSet     as HS
import qualified Data.HashSet     as HashSet
import qualified Data.Heap        as Heap
import           Data.List.Split  (chunksOf)
import qualified Data.Map.Lazy    as Map
import qualified Data.Set         as Set
import           Util             (readLines)

--------------------------------------------------------------------------------
-- Day 15 - Chitons
--------------------------------------------------------------------------------

type Grid = Array Int (Array Int Int)

input :: IO Grid
input = readGrid <$> readLines "inputs/day15.txt"
example :: IO Grid
example = readGrid <$> readLines "inputs/day15-example.txt"

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

djikstras :: Grid -> Int
djikstras grid = search HashSet.empty (Heap.singleton (0, (0, 0)))
  where
    dest = (snd (bounds grid), snd (bounds grid))
    value (i, j) = grid ! i ! j
    search :: HashSet.HashSet (Int, Int) -> Heap.MinPrioHeap Int (Int, Int) -> Int
    search visited toBeVisited = case Heap.view toBeVisited of
      Nothing -> maxBound
      Just ((dist, (i, j)), rest) ->
        if (i, j) `HashSet.member` visited then search visited rest else (
          let
            visited' = HashSet.insert (i, j) visited
            neighbors = filter (not . (`HashSet.member` visited)) $
                        filter (inRange ((0, 0), dest))
                        [ (i + 1, j)
                        , (i - 1, j)
                        , (i, j + 1)
                        , (i, j - 1)
                        ]
            neighborNodes = map (\x -> (dist + value x, x)) neighbors
          in if (i, j) == dest then dist else search visited' (foldr Heap.insert rest neighborNodes))

-- findPath :: Grid -> (Int, Int) -> Int
-- findPath grid start =
--   let
--     neighbors (i, j) = HS.fromList $ [ (x, y) | (x, y) <- [(i-1, j), (i, j-1), (i+1, j), (i, j+1)], x>=0, y>=0, x <= fst start, y <= snd start]
--     weight _ = riskValue grid
--     heur = const 0
--     goal (0, 0) = True
--     goal _      = False
--     path = aStar neighbors weight heur goal start
--     pathSum = sum $ maybe [] (map (riskValue grid)) path
--   in riskValue grid start - riskValue grid (0, 0) + pathSum

part15a :: IO Int
part15a = do
  grid <- input
  let width = snd (bounds grid) + 1
  return $ djikstras grid

coords :: [[(Int, Int)]]
coords = [ [ (i, j) | j <- [0..] ] | i <- [0..] ]

part15b :: IO Int
part15b = do
  grid <- input
  let width = (snd (bounds grid) + 1) * 5
  let bigGrid = listArray (0, width - 1) $ map (listArray (0, width - 1) . map (riskValue grid)) coords
  return $ djikstras bigGrid
