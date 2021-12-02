module Lib
  ( countDepthIncreases,
    countDepthWindowIncreases,
    calculateSimpleCoordinates,
    calculateCoordinates,
  )
where

import Data.List

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

pairsByThree :: [a] -> [(a, a)]
pairsByThree = zip <*> tail . tail . tail

triples :: [a] -> [(a, a, a)]
triples = zip3 <*> tail <*> tail . tail

--------------------------------------------------------------------------------
-- Day 1 - Sonar Sweep
--------------------------------------------------------------------------------

countDepthIncreases :: [Int] -> Int
countDepthIncreases = length . filter (uncurry (<)) . (zip <*> tail)

countDepthWindowIncreases :: [Int] -> Int
countDepthWindowIncreases = length . filter (uncurry (<)) . (zip <*> drop 3)

--------------------------------------------------------------------------------
-- Day 2 - Dive!
--------------------------------------------------------------------------------

applySimpleSubmarineCommand :: (String, Int) -> (Int, Int) -> (Int, Int)
applySimpleSubmarineCommand ("forward", n) (x, y) = (x + n, y)
applySimpleSubmarineCommand ("down", n) (x, y) = (x, y + n)
applySimpleSubmarineCommand ("up", n) (x, y) = (x, y - n)
applySimpleSubmarineCommand _ coord = coord

calculateSimpleCoordinates :: [(String, Int)] -> (Int, Int)
calculateSimpleCoordinates = foldr applySimpleSubmarineCommand (0, 0)

-- Actual submarines need aim, not just moving up and down

applySubmarineCommand :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
applySubmarineCommand (x, y, a) ("forward", n) = (x + n, y + (a * n), a)
applySubmarineCommand (x, y, a) ("down", n) = (x, y, a + n)
applySubmarineCommand (x, y, a) ("up", n) = (x, y, a - n)
applySubmarineCommand coords _ = coords

calculateCoordinates :: [(String, Int)] -> (Int, Int, Int)
calculateCoordinates = foldl' applySubmarineCommand (0, 0, 0)