module Lib
  ( countDepthIncreases,
    countDepthWindowIncreases,
  )
where

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

pairsByThree :: [a] -> [(a, a)]
pairsByThree = zip <*> tail . tail . tail

triples :: [a] -> [(a, a, a)]
triples = zip3 <*> tail <*> tail . tail

countDepthIncreases :: [Int] -> Int
countDepthIncreases = length . filter (\(x, y) -> y > x) . pairs

countDepthWindowIncreases :: [Int] -> Int
countDepthWindowIncreases = length . filter (\(x, y) -> y > x) . pairsByThree
