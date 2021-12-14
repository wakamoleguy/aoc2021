module Day2 (part2a, part2b) where
import           Data.Foldable (foldl')
import           Data.Maybe    (mapMaybe)
import           Text.Read     (readMaybe)
import           Util          (readLines)

--------------------------------------------------------------------------------
-- Day 2 - Dive!
--------------------------------------------------------------------------------

input :: IO [(String, Int)]
input = mapMaybe readCommand <$> readLines "inputs/day2.txt"

readCommand :: String -> Maybe (String, Int)
readCommand s = do
  case words s of
    [c, n] -> (,) c <$> readMaybe n
    _      -> Nothing

checkSumA :: (Int, Int) -> Int
checkSumA = uncurry (*)

checkSumB :: (Int, Int, Int) -> Int
checkSumB (x, y, _) = x * y

applySimpleSubmarineCommand :: (String, Int) -> (Int, Int) -> (Int, Int)
applySimpleSubmarineCommand ("forward", n) (x, y) = (x + n, y)
applySimpleSubmarineCommand ("down", n) (x, y)    = (x, y + n)
applySimpleSubmarineCommand ("up", n) (x, y)      = (x, y - n)
applySimpleSubmarineCommand _ coord               = coord

calculateSimpleCoordinates :: [(String, Int)] -> (Int, Int)
calculateSimpleCoordinates = foldr applySimpleSubmarineCommand (0, 0)

-- Actual submarines need aim, not just moving up and down

applySubmarineCommand :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
applySubmarineCommand (x, y, a) ("forward", n) = (x + n, y + (a * n), a)
applySubmarineCommand (x, y, a) ("down", n)    = (x, y, a + n)
applySubmarineCommand (x, y, a) ("up", n)      = (x, y, a - n)
applySubmarineCommand coords _                 = coords

calculateCoordinates :: [(String, Int)] -> (Int, Int, Int)
calculateCoordinates = foldl' applySubmarineCommand (0, 0, 0)

part2a :: IO Int
part2a = checkSumA . calculateSimpleCoordinates <$> input

part2b :: IO Int
part2b = checkSumB . calculateCoordinates <$> input

