module Day9 (part9a, part9b) where
import           Control.Applicative (liftA2)
import           Data.Array          (Array, bounds, inRange, listArray, (!))
import           Data.Heap           (MaxHeap, empty, insert)
import qualified Data.Heap           as Heap
import qualified Data.Set            as Set
import           Util                (readLines)

--------------------------------------------------------------------------------
-- Day 9 - Smoke Basin
--------------------------------------------------------------------------------
type Grid = Array Int (Array Int Int)

input = do
  lines <- readLines "inputs/day9.txt"
  let ints = map (map (read . (: []))) lines
  return $ listArray (0, 99) (map (listArray (0, 99)) ints)


neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors grid (x, y) = filter valid $ map addDelta [(-1, 0), (1, 0), (0, -1), (0, 1)]
  where
    addDelta (dx, dy) = (x + dx, y + dy)
    valid (x, y) = inRange (bounds grid) x && inRange (bounds $ grid ! x) y && grid ! x ! y /= 9

lowPoints :: Grid -> [(Int, Int)]
lowPoints grid =
  [ (x, y)
    | x <- [0 .. snd (bounds grid)],
      y <- [0 .. snd (bounds $ grid ! x)],
      isLow (x, y),
      value (x, y) /= 9
  ]
  where
    value (x, y) = grid ! x ! y
    isLow p = all (\p' -> value p' > value p) $ neighbors grid p

riskLevel :: Grid -> (Int, Int) -> Int
riskLevel grid (x, y) = 1 + (grid ! x ! y)

-- Part 2

basinSizes :: Grid -> MaxHeap Int
basinSizes grid = foldr (insert . sizeBasin) empty (lowPoints grid)
  where
    ns = neighbors grid
    sizeBasin lowPoint = size' Set.empty [lowPoint] 0
    size' seen [] i = i
    size' seen (p : ps) i =
      if Set.member p seen
        then size' seen ps i
        else size' (Set.insert p seen) (ns p ++ ps) (i + 1)

part9a = sum <$> liftA2 map (riskLevel <$> input)  (lowPoints <$> input)
part9b = product . Heap.take 3 . basinSizes <$> input
