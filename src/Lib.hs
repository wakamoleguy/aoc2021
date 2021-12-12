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
    checkSyntax,
    scoreSyntax,
    scoreAutocomplete,
    median,
    SyntaxResult (..),
    buildGraph,
    paths,
    pathsDoubleVisit,
  )
where

import           Data.Array      (Array, Ix (inRange), bounds, (!))
import           Data.Char       (isLower)
import           Data.Heap       (MaxHeap, empty, insert)
import           Data.List       (find, foldl', sort, transpose)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import qualified Data.Set        as Set
import           Debug.Trace     ()

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
        Just b  -> n * score b
        Nothing -> playBingo ns stampedBoards

stamp :: Int -> BingoBoard -> BingoBoard
stamp n (rs, cs) = (map f rs, map f cs)
  where
    f :: [Int] -> [Int]
    f = filter (n /=)

isWinner :: BingoBoard -> Bool
isWinner ([] : rs, cs)    = True
isWinner (rs, [] : cs)    = True
isWinner ([], [])         = False
isWinner (r : rs, c : cs) = isWinner (rs, cs)
isWinner _                = error "Invalid board - unequal rows and cols"

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
        _  -> loseAtBingo ns losingBoards
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
        then dangerousPoints' ps' seen (p `Set.insert` ds)
        else dangerousPoints' ps' (p `Set.insert` seen) ds

findDangerousPoints :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
findDangerousPoints = dangerousPoints . concatMap (uncurry pointsBetween)

--------------------------------------------------------------------------------
-- Day 6 - Lantern Fish
--------------------------------------------------------------------------------

to9Tuple :: [a] -> (a, a, a, a, a, a, a, a, a)
to9Tuple [x, y, z, w, a, b, c, d, e] = (x, y, z, w, a, b, c, d, e)
to9Tuple _                           = error "Invalid tuple"

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
type Grid = Array Int (Array Int Int)

neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors grid (x, y) = filter valid $ map addDelta [(-1, 0), (1, 0), (0, -1), (0, 1)]
  where
    addDelta (dx, dy) = (x + dx, y + dy)
    valid (x, y) = inRange (bounds grid) x && inRange (bounds $ grid ! x) y && grid ! x ! y /= 9

lowPoints :: Grid -> [(Int, Int)]
lowPoints grid =
  [ (x, y)
    | x <- [0 .. snd (bounds grid)],
      y <- [0 .. snd (bounds $ grid ! x)],
      isLow (x, y),
      value (x, y) /= 9
  ]
  where
    value (x, y) = grid ! x ! y
    isLow p = all (\p' -> value p' > value p) $ neighbors grid p

riskLevel :: Grid -> (Int, Int) -> Int
riskLevel grid (x, y) = 1 + (grid ! x ! y)

-- Part 2

basinSizes :: Grid -> MaxHeap Int
basinSizes grid = foldr (insert . sizeBasin) empty (lowPoints grid)
  where
    ns = neighbors grid
    sizeBasin lowPoint = size' Set.empty [lowPoint] 0
    size' seen [] i = i
    size' seen (p : ps) i =
      if Set.member p seen
        then size' seen ps i
        else size' (Set.insert p seen) (ns p ++ ps) (i + 1)

--------------------------------------------------------------------------------
-- Day 10 - Syntax Scoring
--------------------------------------------------------------------------------
data SyntaxResult = OK | Corrupt Char | Incomplete String

checkSyntax :: String -> SyntaxResult
checkSyntax s = checkSyntax' s []
  where
    checkSyntax' [] [] = OK
    checkSyntax' ('(' : xs) stack = checkSyntax' xs (')' : stack)
    checkSyntax' ('[' : xs) stack = checkSyntax' xs (']' : stack)
    checkSyntax' ('{' : xs) stack = checkSyntax' xs ('}' : stack)
    checkSyntax' ('<' : xs) stack = checkSyntax' xs ('>' : stack)
    checkSyntax' (closer : xs) (expected : stack) = if closer == expected then checkSyntax' xs stack else Corrupt closer
    checkSyntax' [] stack = Incomplete stack
    checkSyntax' (x : xs) stack = Corrupt x

scoreSyntax :: SyntaxResult -> Int
scoreSyntax (Corrupt ')') = 3
scoreSyntax (Corrupt ']') = 57
scoreSyntax (Corrupt '}') = 1197
scoreSyntax (Corrupt '>') = 25137
scoreSyntax _             = 0

scoreAutocomplete :: SyntaxResult -> Int
scoreAutocomplete (Incomplete stack) = foldl' (\agg char -> agg * 5 + score char) 0 stack
  where
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4
    score _   = 0
scoreAutocomplete _ = 0

median :: [Int] -> Int
median xs =
  let sorted = sort xs
      len = length sorted
   in if odd len
        then sorted !! (len `div` 2)
        else (sorted !! (len `div` 2 - 1) + sorted !! (len `div` 2)) `div` 2

--------------------------------------------------------------------------------
-- Day 11 - Passage Pathing
--------------------------------------------------------------------------------
data Cave = Big String | Small String deriving (Eq, Show, Ord)

-- 1. Read input as string pairs
readEdge :: String -> (Cave, Cave)
readEdge s = (stringToCave $ head parts, stringToCave $ last parts)
  where
    parts = splitOn "-" s
    stringToCave s = if isLower (head s) then Small s else Big s

-- 2. Edges are bidirectional, so store them as (from, to) and (to, from)
type Graph = Map.Map Cave (Set.Set Cave)

addEdge :: Graph -> (Cave, Cave) -> Graph
addEdge graph (from, to) =
  let fromEdges = Map.findWithDefault Set.empty from graph
      toEdges = Map.findWithDefault Set.empty to graph
   in Map.insert from (Set.insert to fromEdges) $ Map.insert to (Set.insert from toEdges) graph

graphNeighbors :: Graph -> Cave -> Set.Set Cave
graphNeighbors graph = Set.delete (Small "start") . flip (Map.findWithDefault Set.empty) graph

-- 3. Reduce them into a Map, so I can call neighbors on them.
buildGraph :: [String] -> Graph
buildGraph lines = foldl' addEdge Map.empty $ map readEdge lines

-- 4. DFS from start to end??
paths :: Graph -> [[Cave]]
paths graph =
  let path' :: Set.Set Cave -> Cave -> [[Cave]]
      path' _ (Small "end") = [[Big "end"]]
      path' seen c =
        let neighbors = graphNeighbors graph c
            unseen = neighbors Set.\\ seen
            seen' = case c of
              Small _ -> Set.insert c seen
              Big _   -> seen
         in concatMap (map (c :) . path' seen') unseen
  in path' Set.empty (Small "start")

pathsDoubleVisit :: Graph -> [[Cave]]
pathsDoubleVisit graph =
  let path' :: Set.Set Cave -> Bool -> Cave -> [[Cave]]
      path' _ _ (Small "end") = [[Small "end"]]
      path' seen isDbl c@(Big _) =
        let neighbors = graphNeighbors graph c
         in concatMap (map (c :) . path' seen isDbl) neighbors
      path' seen False c@(Small _) =
        let neighbors = graphNeighbors graph c
            isDbl' = Set.member c seen
            seen' = Set.insert c seen
         in concatMap (map (c :) . path' seen' isDbl') neighbors
      path' seen True c@(Small _) =
        let neighbors = graphNeighbors graph c
            isDbl' = Set.member c seen
            seen' = Set.insert c seen
         in if isDbl' then [] else concatMap (map (c :) . path' seen' True) neighbors
  in path' Set.empty False (Small "start")
