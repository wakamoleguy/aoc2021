module Day3 (part3a, part3b) where
import           Control.Applicative (Applicative (liftA2))
import           Data.Foldable       (foldl')
import           Data.List           (transpose)
import           Util                (readLines)

--------------------------------------------------------------------------------
-- Day 3 - Binary Diagnostic
--------------------------------------------------------------------------------

input :: IO [String]
input = readLines "inputs/day3.txt"

parseBinary :: String -> [Int]
parseBinary = map (read . (: []))

mostCommonBit :: [Int] -> Int
mostCommonBit = sumToDigit . foldl' (\x y -> x + 2 * y - 1) 0
  where
    sumToDigit s = if s >= 0 then 1 else 0

leastCommonBit :: [Int] -> Int
leastCommonBit = (1 -) . mostCommonBit

binaryToDecimal :: [Int] -> Int
binaryToDecimal = foldl' (\x y -> x * 2 + y) 0

powerConsumption :: [String] -> Int
powerConsumption bs = binaryToDecimal gammaRates * binaryToDecimal etaRates
  where
    gammaRates = map mostCommonBit $ transpose $ map parseBinary bs
    etaRates = map (1 -) gammaRates

-- Part 2
calculateFilterCode :: ([Int] -> Int) -> [String] -> Int
calculateFilterCode targeter = binaryToDecimal . calculate' . map parseBinary
  where
    calculate' :: [[Int]] -> [Int]
    calculate' [b] = b
    calculate' bs =
      let targetBit = targeter $ map head bs
          validCodes = filter ((targetBit ==) . head) bs
       in targetBit : calculate' (map tail validCodes)

oxygenConsumption :: [String] -> Int
oxygenConsumption = calculateFilterCode mostCommonBit

c02Consumption :: [String] -> Int
c02Consumption = calculateFilterCode leastCommonBit

part3a :: IO Int
part3a = powerConsumption <$> input
part3b :: IO Int
part3b = liftA2 (*) (oxygenConsumption <$> input) (c02Consumption <$> input)
