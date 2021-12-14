module Day1 (part1a, part1b) where
import           Util (readLines)

--------------------------------------------------------------------------------
-- Day 1 - Sonar Sweep
--------------------------------------------------------------------------------

input :: IO [Int]
input = map read <$> readLines "inputs/day1.txt"

part1a :: IO Int
part1a = countDepthIncreases <$> input

part1b :: IO Int
part1b = countDepthWindowIncreases <$> input

countDepthIncreases :: [Int] -> Int
countDepthIncreases = length . filter (uncurry (<)) . (zip <*> tail)

countDepthWindowIncreases :: [Int] -> Int
countDepthWindowIncreases = length . filter (uncurry (<)) . (zip <*> drop 3)
