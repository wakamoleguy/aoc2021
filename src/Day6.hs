module Day6 (part6a, part6b) where
import           Control.Applicative (liftA2)
import           Util                (readCommaSeparatedInts, readLines)

--------------------------------------------------------------------------------
-- Day 6 - Lantern Fish
--------------------------------------------------------------------------------

input = readCommaSeparatedInts . head <$> readLines "inputs/day6.txt"

to9Tuple :: [a] -> (a, a, a, a, a, a, a, a, a)
to9Tuple [x, y, z, w, a, b, c, d, e] = (x, y, z, w, a, b, c, d, e)
to9Tuple _                           = error "Invalid tuple"

simulateLanternfish :: [Int] -> Int -> Int
simulateLanternfish fishes days =
  let fishCounts = to9Tuple [length (filter (== i) fishes) | i <- [0 .. 8]]
      simulate' (a, b, c, d, e, f, g, h, i) 0 = a + b + c + d + e + f + g + h + i
      simulate' (a, b, c, d, e, f, g, h, i) n = simulate' (b, c, d, e, f, g, h + a, i, a) (n -1)
   in simulate' fishCounts days

part6a = flip simulateLanternfish 80 <$> input
part6b = flip simulateLanternfish 256 <$> input
