module Lib
  ( countDepthIncreases,
    countDepthWindowIncreases,
    calculateSimpleCoordinates,
    calculateCoordinates,
    powerConsumption,
    parseBinary,
    oxygenConsumption,
    c02Consumption,
    playBingo,
    loseAtBingo,
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

--------------------------------------------------------------------------------
-- Day 3 - Binary Diagnostic
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Day 4 - Giant Squid
--------------------------------------------------------------------------------

-- ...wants to play bingo?

-- We store the transposed board for convenience
type BingoBoard = ([[Int]], [[Int]])

playBingo :: [Int] -> [BingoBoard] -> Int
playBingo [] _ = 0
playBingo (n:ns) bs =
  let stampedBoards = map (stamp n) bs
      winningBoard = find isWinner stampedBoards
  in case winningBoard of
    Just b -> n * score b
    Nothing -> playBingo ns stampedBoards

stamp :: Int -> BingoBoard -> BingoBoard
stamp n (rs, cs) = (map f rs, map f cs)
  where
    f :: [Int] -> [Int]
    f = filter (n /=)

isWinner :: BingoBoard -> Bool
isWinner ([]:rs, cs) = True
isWinner (rs, []:cs) = True
isWinner ([], []) = False
isWinner (r:rs, c:cs) = isWinner (rs, cs)

score :: BingoBoard -> Int
score = sum . map sum . fst

loseAtBingo :: [Int] -> [BingoBoard] -> Int
loseAtBingo [] _ = 0
loseAtBingo (n:ns) bs@(b:_) =
  let stampedBoards = map (stamp n) bs
      winningBoards = filter isWinner stampedBoards
      losingBoards = filter (not . isWinner) stampedBoards
  in case losingBoards of
    [] -> n * (score $ head winningBoards)
    _ -> loseAtBingo ns losingBoards