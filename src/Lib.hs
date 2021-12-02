module Lib
  ( countDepthIncreases,
    countDepthWindowIncreases,
    calculateCoordinates,
  )
where

import Data.Semigroup

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

pairsByThree :: [a] -> [(a, a)]
pairsByThree = zip <*> tail . tail . tail

triples :: [a] -> [(a, a, a)]
triples = zip3 <*> tail <*> tail . tail

-- Day 1 - Sonar Sweep

countDepthIncreases :: [Int] -> Int
countDepthIncreases = length . filter (uncurry (<)) . (zip <*> tail)

countDepthWindowIncreases :: [Int] -> Int
countDepthWindowIncreases = length . filter (uncurry (<)) . (zip <*> drop 3)

-- Day 2 - Dive!

applySubmarineCommand :: (String, Int) -> (Int, Int) -> (Int, Int)
applySubmarineCommand ("forward", n) (x, y) = (x + n, y)
applySubmarineCommand ("down", n) (x, y) = (x, y + n)
applySubmarineCommand ("up", n) (x, y) = (x, y - n)
applySubmarineCommand _ coord = coord

calculateCoordinates :: [(String, Int)] -> (Int, Int)
calculateCoordinates = foldr applySubmarineCommand (0, 0)