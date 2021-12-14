module Day5 (part5a, part5b) where
import           Data.List.Split (dropDelims, oneOf, split)
import           Data.Maybe      (mapMaybe)
import qualified Data.Set        as Set
import           Text.Read       (readMaybe)
import           Util            (readLines)

--------------------------------------------------------------------------------
-- Day 5 - Hydrothermal Venture
--------------------------------------------------------------------------------

readPoints :: String -> ((Int, Int), (Int, Int))
readPoints = zipTuple . mapMaybe readMaybe . split (dropDelims $ oneOf ", ->")
  where
    zipTuple (x : y : x' : y' : _) = ((x, y), (x', y'))
    zipTuple _                     = error "Invalid input"

input :: IO [((Int, Int), (Int, Int))]
input = map readPoints <$> readLines "inputs/day5.txt"

isCardinal :: ((Int, Int), (Int, Int)) -> Bool
isCardinal ((x, y), (x', y')) = x == x' || y == y'

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween (x, y) (x', y') =
  let xs = if x == x' then repeat x else [x, x + signum (x' - x) .. x']
      ys = if y == y' then repeat y else [y, y + signum (y' - y) .. y']
   in zip xs ys

dangerousPoints :: [(Int, Int)] -> [(Int, Int)]
dangerousPoints ps = Set.toList $ dangerousPoints' ps Set.empty Set.empty
  where
    dangerousPoints' [] seen ds = ds
    dangerousPoints' (p : ps') seen ds =
      if p `Set.member` seen
        then dangerousPoints' ps' seen (p `Set.insert` ds)
        else dangerousPoints' ps' (p `Set.insert` seen) ds

findDangerousPoints :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
findDangerousPoints = dangerousPoints . concatMap (uncurry pointsBetween)

part5a :: IO Int
part5a = length . findDangerousPoints . filter isCardinal <$> input
part5b :: IO Int
part5b = length . findDangerousPoints <$> input
