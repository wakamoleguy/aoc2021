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
    isCardinal,
    pointsBetween,
    dangerousPoints,
    findDangerousPoints,
    simulateLanternfish,
    fuelToAlignCrabs,
    nonLinearFuelToAlignCrabs,
    countDigits1478,
    decodeDigits,
    decodeOutput,
    decode,
    riskLevel,
    lowPoints,
    basinSizes,
  )
where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

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
playBingo (n : ns) bs =
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
isWinner ([] : rs, cs) = True
isWinner (rs, [] : cs) = True
isWinner ([], []) = False
isWinner (r : rs, c : cs) = isWinner (rs, cs)
isWinner _ = error "Invalid board - unequal rows and cols"

score :: BingoBoard -> Int
score = sum . map sum . fst

loseAtBingo :: [Int] -> [BingoBoard] -> Int
loseAtBingo [] _ = 0
loseAtBingo (n : ns) bs@(b : _) =
  let stampedBoards = map (stamp n) bs
      winningBoards = filter isWinner stampedBoards
      losingBoards = filter (not . isWinner) stampedBoards
   in case losingBoards of
        [] -> n * score (head winningBoards)
        _ -> loseAtBingo ns losingBoards
loseAtBingo _ _ = error "Invalid game - No boards to play"

--------------------------------------------------------------------------------
-- Day 5 - Hydrothermal Venture
--------------------------------------------------------------------------------

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
        then dangerousPoints' ps' (p `Set.insert` seen) (p `Set.insert` ds)
        else dangerousPoints' ps' (p `Set.insert` seen) ds

findDangerousPoints :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
findDangerousPoints = dangerousPoints . concatMap (uncurry pointsBetween)

--------------------------------------------------------------------------------
-- Day 6 - Lantern Fish
--------------------------------------------------------------------------------

to9Tuple :: [a] -> (a, a, a, a, a, a, a, a, a)
to9Tuple [x, y, z, w, a, b, c, d, e] = (x, y, z, w, a, b, c, d, e)
to9Tuple _ = error "Invalid tuple"

simulateLanternfish :: [Int] -> Int -> Int
simulateLanternfish fishes days =
  let fishCounts = to9Tuple [length (filter (== i) fishes) | i <- [0 .. 8]]
      simulate' (a, b, c, d, e, f, g, h, i) 0 = a + b + c + d + e + f + g + h + i
      simulate' (a, b, c, d, e, f, g, h, i) n = simulate' (b, c, d, e, f, g, h + a, i, a) (n -1)
   in simulate' fishCounts days

--------------------------------------------------------------------------------
-- Day 7 - The Treachery of Whales
--------------------------------------------------------------------------------

fuel' :: (Int -> Int -> Int) -> [Int] -> Int
fuel' fueler crabs =
  let minCrab = minimum crabs
      maxCrab = maximum crabs
      alignToPosition x = sum $ map (fueler x) crabs
   in minimum $ map alignToPosition [minCrab .. maxCrab]

-- TODO - Prove to myself that median minimizes this linear distance between points
fuelToAlignCrabs :: [Int] -> Int
fuelToAlignCrabs = fuel' fueler
  where
    fueler x y = abs (x - y)

-- TODO - Prove to myself that mean minimizes this quadratic distance between points
nonLinearFuelToAlignCrabs :: [Int] -> Int
nonLinearFuelToAlignCrabs = fuel' fueler
  where
    fueler x y = abs (x - y) * (abs (x - y) + 1) `div` 2

--------------------------------------------------------------------------------
-- Day 8 - Seven Segment Search
--------------------------------------------------------------------------------

countDigits1478 :: [String] -> Int
countDigits1478 = length . filter (flip elem [2, 3, 4, 7] . length)

-- Part 2
-- Position = encoded value:
--  0 = 6 letter group that has all 3 letters of 3 letter group, but not all 4 of 4 letter group
--  1 = 2 letter group
--  2 = 5 letter group that has segment that digit 8 has over digit 9 (segment e)
--  3 = 5 letter group that has all 3 letters of 3 letter group
--  4 = 4 letter group
--  5 = 5 letter group that...is not digit 2 or digit 3? - has segment that digit 3 has that digit 2 does not
--  6 = 6 letter group that does not have both letters of 2 letter group
--  7 = 3 letter group
--  8 = 7 letter group
--  9 = 6 letter group that has all 4 letters of 4 letter group

decodeDigits :: [String] -> (String, String, String, String, String, String, String, String, String, String)
decodeDigits digits =
  let zero = head $ filter (\x -> length x == 6 && all (`elem` x) seven && not (all (`elem` x) four)) digits
      one = head $ filter (\x -> length x == 2) digits
      two = head $ filter (\x -> length x == 5 && missing89Segment `elem` x) digits
      three = head $ filter (\x -> length x == 5 && all (`elem` x) seven) digits
      four = head $ filter (\x -> length x == 4) digits
      five = head $ filter (\x -> length x == 5 && x /= two && x /= three) digits
      six = head $ filter (\x -> length x == 6 && not (all (`elem` x) one)) digits
      seven = head $ filter (\x -> length x == 3) digits
      eight = head $ filter (\x -> length x == 7) digits
      nine = head $ filter (\x -> length x == 6 && all (`elem` x) four) digits
      missing89Segment = head $ filter (`notElem` nine) eight
   in (sort zero, sort one, sort two, sort three, sort four, sort five, sort six, sort seven, sort eight, sort nine)

decodeOutput :: (String, String, String, String, String, String, String, String, String, String) -> String -> Int
decodeOutput (a, b, c, d, e, f, g, h, i, j) x
  | a == sort x = 0
  | b == sort x = 1
  | c == sort x = 2
  | d == sort x = 3
  | e == sort x = 4
  | f == sort x = 5
  | g == sort x = 6
  | h == sort x = 7
  | i == sort x = 8
  | j == sort x = 9
  | otherwise = error ("Invalid digit" ++ show x ++ " " ++ show (a, b, c, d, e, f, g, h, i, j))

decode :: [String] -> [String] -> Int
decode digits = foldl' (\b a -> b * 10 + decode' a) 0
  where
    decode' = decodeOutput (decodeDigits digits)

--------------------------------------------------------------------------------
-- Day 9 - Smoke Basin
--------------------------------------------------------------------------------
wrapGrid :: [[a]] -> [[Maybe a]]
wrapGrid a = maybeRow : map wrappedRow a ++ [maybeRow]
  where
    gridLength = length $ head a
    maybeRow = replicate (gridLength + 2) Nothing
    wrappedRow row = Nothing : map Just row ++ [Nothing]

rowWindows :: [[a]] -> [([a], [a], [a])]
rowWindows = concatMap columnWindows . triples
  where
    columnWindows (a, b, c) = triples $ transpose [a, b, c]

lowPoint :: ([Maybe Int], [Maybe Int], [Maybe Int]) -> Maybe Int
lowPoint ([_, up, _], [left, Just center, right], [_, down, _]) =
  let getMaybe (Just x) = x
      getMaybe Nothing = maxBound
      isLowPoint = all ((> center) . getMaybe) [up, left, right, down]
   in if isLowPoint then Just center else Nothing
lowPoint _ = Nothing

lowPoints :: [[Int]] -> [Int]
lowPoints grid = mapMaybe lowPoint $ rowWindows $ wrapGrid grid

riskLevel :: Int -> Int
riskLevel = (1 +)

-- Part 2

basinSizes :: [[Int]] -> [Int]
basinSizes grid = sizes' allPoints Set.empty [] 0
  where
    numRows = length grid
    numCols = length $ head grid
    valid (x, y) = x >= 0 && x < numRows && y >= 0 && y < numCols && grid !! x !! y /= 9
    neighbors (x, y) = filter valid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    allPoints = filter valid $ do
      r <- [0 .. numRows - 1]
      c <- [0 .. numCols - 1]
      return (r, c)
    sizes' (p : ps) seen [] i = if i == 0 then sizes' ps seen [p] 0 else i : sizes' ps seen [p] 0
    sizes' [] seen [] i = [i | i /= 0]
    sizes' rest seen (p : ps) i =
      if Set.member p seen
        then sizes' rest seen ps i
        else
          ( let neighbors' = filter (`Set.notMember` seen) $ neighbors p
                seen' = Set.insert p seen
             in sizes' rest seen' (neighbors' ++ ps) (i + 1)
          )
