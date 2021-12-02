module Lib
  ( countDepthIncreases,
    countDepthWindowIncreases,
  )
where

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

triples :: [a] -> [(a, a, a)]
triples = zip3 <*> tail <*> tail . tail

countDepthIncreases :: [Int] -> Int
countDepthIncreases = length . filter (\(x, y) -> y > x) . pairs

countDepthWindowIncreases :: [Int] -> Int
countDepthWindowIncreases = countDepthIncreases . map (\(a, b, c) -> a + b + c) . triples
