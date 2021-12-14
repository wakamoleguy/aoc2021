module Day7 (part7a, part7b) where
import           Util (readCommaSeparatedInts, readLines)

--------------------------------------------------------------------------------
-- Day 7 - The Treachery of Whales
--------------------------------------------------------------------------------
input = readCommaSeparatedInts . head <$> readLines "inputs/day7.txt"

fuel' :: (Int -> Int -> Int) -> [Int] -> Int
fuel' fueler crabs =
  let minCrab = minimum crabs
      maxCrab = maximum crabs
      alignToPosition x = sum $ map (fueler x) crabs
   in minimum $ map alignToPosition [minCrab .. maxCrab]

-- TODO - Prove to myself that median minimizes this linear distance between points
fuelToAlignCrabs :: [Int] -> Int
fuelToAlignCrabs = fuel' fueler
  where
    fueler x y = abs (x - y)

-- TODO - Prove to myself that mean minimizes this quadratic distance between points
nonLinearFuelToAlignCrabs :: [Int] -> Int
nonLinearFuelToAlignCrabs = fuel' fueler
  where
    fueler x y = abs (x - y) * (abs (x - y) + 1) `div` 2

part7a = fuelToAlignCrabs <$> input
part7b = nonLinearFuelToAlignCrabs <$> input
