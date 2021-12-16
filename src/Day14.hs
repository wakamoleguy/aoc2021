module Day14 (part14a, part14b) where
import           Control.Applicative (liftA2)
import           Data.Foldable       (foldl')
import           Data.List           (group, sort)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (listToMaybe, mapMaybe)
import           Util                (readLines)

--------------------------------------------------------------------------------
-- Day 14 - Extended Polymerization
--------------------------------------------------------------------------------

input = readLines "inputs/day14.txt"
polymerBase = (++ "x") . head <$> input
polymerInsertionRules =
  let rules lines = tail $ tail lines
      splitRule [a, c, _, _, _, _, b] = (a, b, c)
      splitRule _                     = undefined
  in map splitRule . rules <$> input

basePairs :: String -> Map.Map (Char, Char) Int
basePairs s = Map.fromList $ map (\pairs -> (head pairs, length pairs)) $ group $ sort $ zip s $ tail s

applyRules m [] = Map.empty
applyRules m ((a, b, c):rs) =
  let ac = Map.findWithDefault 0 (a, c) m
      rest = applyRules m rs
  in Map.insertWith (+) (a, b) ac $ Map.insertWith (+) (b, c) ac $  Map.insertWith (+) (a, c) (-ac) rest

applySteps 0 rules base = base
applySteps n rules base = applySteps (n - 1) rules $ Map.filter (>0) $ combineDelta base (applyRules base rules)
  where combineDelta m1 m2 = Map.unionWith (+) m1 m2

countLetters :: Map.Map (Char, Char) Int -> Int
countLetters m =
  let entries = map (\((k, _), n) -> (k, n)) $ Map.toList m
      byLetter = filter (/= 0) [ sum $ map snd $ filter (\(k, v) -> k == c) entries | c <- ['A' .. 'Z'] ]
  in maximum byLetter - minimum byLetter

part14a :: IO Int
part14a = do
  rules <- polymerInsertionRules
  base <- polymerBase
  let baseMap = basePairs base
  let polymer = applySteps 10 rules baseMap
  return $ countLetters polymer
part14b :: IO Int
part14b = do
  rules <- polymerInsertionRules
  base <- polymerBase
  let baseMap = basePairs base
  let polymer = applySteps 40 rules baseMap
  return $ countLetters polymer
