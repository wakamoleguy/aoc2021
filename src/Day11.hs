module Day11 (part11a, part11b) where
import           Data.Array    (Array, bounds, elems, inRange, listArray, (!),
                                (//))
import           Data.Foldable (foldl')
import           Util          (readLines)


--------------------------------------------------------------------------------
-- Day 11 - Dumbo Octopus
--------------------------------------------------------------------------------

-- Add 1 to all octopuses
-- From 0, 0 to max, max
--  Maybe flash octopus
--
-- maybe flash octopus:
--  if energy < 9, return
--  return
input = do
  lines <- readLines "inputs/day11.txt"
  let grid = map (map (read . (: []))) lines
  return $ listArray (0, 9) (map (listArray (0, 9)) grid)

type Grid = Array Int (Array Int Int)

neighborsWithDiagonals :: Grid -> (Int, Int) -> [(Int, Int)]
neighborsWithDiagonals grid (x, y) = filter valid $ map addDelta [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  where
    addDelta (dx, dy) = (x + dx, y + dy)
    valid (x, y) = inRange (bounds grid) x && inRange (bounds $ grid ! x) y

setInGrid :: Grid -> (Int, Int) -> Int -> Grid
setInGrid grid (x, y) val = grid // [(x, grid ! x // [(y, val)])]

flashOctopus :: Grid -> (Int, Int) -> Grid
flashOctopus grid (x, y)
  | grid ! x ! y == 0 = grid
  | grid ! x ! y <= 9 = grid
  | otherwise =
    let ns = filter (not . alreadyFlashed) $ neighborsWithDiagonals grid (x, y)
        alreadyFlashed (x, y) = grid ! x ! y == 0
        incrementNeighbors = foldl' (\g (x, y) -> setInGrid g (x, y) ((grid ! x ! y) + 1)) grid ns
     in foldl' flashOctopus (setInGrid incrementNeighbors (x, y) 0) ns

printGrid :: Grid -> String
printGrid grid = foldl' (\b a -> b ++ a ++ "\n") "" $ fmap (foldl' (\b a -> b ++ show a) "") grid

dumboStep :: Grid -> Grid
dumboStep grid = grid'
  where
    incremented = fmap (fmap (+ 1)) grid
    grid' = foldl' flashOctopus incremented [(x, y) | x <- [0 .. snd (bounds grid)], y <- [0 .. snd (bounds $ grid ! x)]]

countDumboFlashesInGrid :: Grid -> Int
countDumboFlashesInGrid = length . filter (== 0) . concatMap elems . elems . dumboStep

countDumboFlashes :: Int -> Grid -> Int
countDumboFlashes n = sum . map countDumboFlashesInGrid . take n . iterate dumboStep

findSynchronized :: Grid -> Int
findSynchronized = fst . head . filter (\(_, n) -> n == 100) . zip [1 ..] . map countDumboFlashesInGrid . iterate dumboStep

part11a = countDumboFlashes 100 <$> input
part11b = findSynchronized <$> input
