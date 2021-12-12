import Data.Array (listArray)
import qualified Data.Heap as Heap
import Data.List (sort, transpose)
import Data.List.Split (dropDelims, oneOf, split)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Lib
  ( SyntaxResult (..),
    basinSizes,
    c02Consumption,
    calculateCoordinates,
    calculateSimpleCoordinates,
    checkSyntax,
    countDepthIncreases,
    countDepthWindowIncreases,
    countDigits1478,
    dangerousPoints,
    decode,
    decodeDigits,
    decodeOutput,
    findDangerousPoints,
    fuelToAlignCrabs,
    isCardinal,
    loseAtBingo,
    lowPoints,
    median,
    nonLinearFuelToAlignCrabs,
    oxygenConsumption,
    playBingo,
    pointsBetween,
    powerConsumption,
    riskLevel,
    scoreAutocomplete,
    scoreSyntax,
    simulateLanternfish,
    buildGraph,
    paths,
    pathsDoubleVisit,
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Printf (PrintfArg (parseFormat))
import Text.Read (readMaybe)

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    it "solves a simple Day 1A example" $ do
      let input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
      countDepthIncreases input `shouldBe` 7

    it "solves Day1A" $ do
      expected <- countDepthIncreases <$> readInput "inputs/day1.txt"
      expected `shouldBe` 1215

    it "solves a simple Day 1B example" $ do
      let input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
      countDepthWindowIncreases input `shouldBe` 5

    it "solves Day1B" $ do
      expected <- countDepthWindowIncreases <$> readInput "inputs/day1.txt"
      expected `shouldBe` 1150

  describe "Day 2" $ do
    let checkSumA = uncurry (*)

    it "solves a simple Day 2A example" $ do
      let input =
            [ ("forward", 5),
              ("down", 5),
              ("forward", 8),
              ("up", 3),
              ("down", 8),
              ("forward", 2)
            ]
      checkSumA (calculateSimpleCoordinates input) `shouldBe` 150

    it "solves Day2A" $ do
      expected <- calculateSimpleCoordinates . mapMaybe readCommand <$> readLines "inputs/day2.txt"
      checkSumA expected `shouldBe` 1804520

    let checkSumB (x, y, _) = x * y

    it "solves a simple Day 2B example" $ do
      let input =
            [ ("forward", 5),
              ("down", 5),
              ("forward", 8),
              ("up", 3),
              ("down", 8),
              ("forward", 2)
            ]
      calculateCoordinates input `shouldBe` (15, 60, 10)
      checkSumB (calculateCoordinates input) `shouldBe` 900

    it "solves Day2B" $ do
      expected <- calculateCoordinates . mapMaybe readCommand <$> readLines "inputs/day2.txt"
      expected `shouldBe` (1970, 1000556, 916)
      checkSumB expected `shouldBe` 1971095320

  describe "Day 3" $ do
    let example =
          [ "00100",
            "11110",
            "10110",
            "10111",
            "10101",
            "01111",
            "00111",
            "11100",
            "10000",
            "11001",
            "00010",
            "01010"
          ]
    it "solves a simple Day 3A example" $ do
      powerConsumption example `shouldBe` 198

    it "solves Day3A" $ do
      input <- readLines "inputs/day3.txt"
      powerConsumption input `shouldBe` 2648450

    it "calculates oxygen consumption for the example" $ do
      oxygenConsumption example `shouldBe` 23
    it "calculate C02 consumption for the example" $ do
      c02Consumption example `shouldBe` 10

    it "solves Day3B" $ do
      input <- readLines "inputs/day3.txt"
      oxygenConsumption input * c02Consumption input `shouldBe` 2845944

  describe "Day 4" $ do
    it "solves a simple Day 4A example" $ do
      example <- readLines "inputs/day4-example.txt"
      let bingoCall = readCommaSeparatedInts $ head example
      let bingoBoards = readBingoBoards $ map readWhitespaceSeparatedInts $ tail example
      playBingo bingoCall bingoBoards `shouldBe` 4512

    it "solves Day 4A" $ do
      input <- readLines "inputs/day4.txt"
      let bingoCall = readCommaSeparatedInts $ head input
      let bingoBoards = readBingoBoards $ map readWhitespaceSeparatedInts $ tail input
      playBingo bingoCall bingoBoards `shouldBe` 8136

    it "solves a simple Day 4B example" $ do
      example <- readLines "inputs/day4-example.txt"
      let bingoCall = readCommaSeparatedInts $ head example
      let bingoBoards = readBingoBoards $ map readWhitespaceSeparatedInts $ tail example
      loseAtBingo bingoCall bingoBoards `shouldBe` 1924

    it "solves Day 4B" $ do
      input <- readLines "inputs/day4.txt"
      let bingoCall = readCommaSeparatedInts $ head input
      let bingoBoards = readBingoBoards $ map readWhitespaceSeparatedInts $ tail input
      loseAtBingo bingoCall bingoBoards `shouldBe` 12738

  describe "Day 5" $ do
    let example =
          [ ((0, 9), (5, 9)),
            ((8, 0), (0, 8)),
            ((9, 4), (3, 4)),
            ((2, 2), (2, 1)),
            ((7, 0), (7, 4)),
            ((6, 4), (2, 0)),
            ((0, 9), (2, 9)),
            ((3, 4), (1, 4)),
            ((0, 0), (8, 8)),
            ((5, 5), (8, 2))
          ]
    it "solves a simple Day 5A example" $ do
      length (filter isCardinal example) `shouldBe` 6
      uncurry pointsBetween (head example) `shouldBe` [(0, 9), (1, 9), (2, 9), (3, 9), (4, 9), (5, 9)]
      pointsBetween (9, 4) (3, 4) `shouldBe` [(9, 4), (8, 4), (7, 4), (6, 4), (5, 4), (4, 4), (3, 4)]
      (length . findDangerousPoints . filter isCardinal) example `shouldBe` 5

    it "solves Day 5A" $ do
      input <- readLines "inputs/day5.txt"
      let parsed = map readPoints input
      (length . findDangerousPoints . filter isCardinal) parsed `shouldBe` 6564

    it "solves a simple Day 5B example" $ do
      length (findDangerousPoints example) `shouldBe` 12
    it "solves Day 5B" $ do
      input <- readLines "inputs/day5.txt"
      let parsed = map readPoints input
      length (findDangerousPoints parsed) `shouldBe` 19172

  describe "Day 6" $ do
    let example = [3, 4, 3, 1, 2]

    it "solves a simple Day 6A example" $ do
      simulateLanternfish example 0 `shouldBe` 5
      simulateLanternfish example 1 `shouldBe` 5
      simulateLanternfish example 2 `shouldBe` 6
      simulateLanternfish example 3 `shouldBe` 7
      simulateLanternfish example 4 `shouldBe` 9
      simulateLanternfish example 5 `shouldBe` 10
      simulateLanternfish example 80 `shouldBe` 5934

    it "solves Day 6A" $ do
      input <- readCommaSeparatedInts . head <$> readLines "inputs/day6.txt"
      simulateLanternfish input 80 `shouldBe` 371379

    it "solves Day 6B" $ do
      simulateLanternfish example 256 `shouldBe` 26984457539
      input <- readCommaSeparatedInts . head <$> readLines "inputs/day6.txt"
      simulateLanternfish input 256 `shouldBe` 1674303997472

  describe "Day 7" $ do
    let example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

    it "solves a simple Day 7A example" $ do
      fuelToAlignCrabs example `shouldBe` 37

    it "solves Day 7A" $ do
      input <- readCommaSeparatedInts . head <$> readLines "inputs/day7.txt"
      fuelToAlignCrabs input `shouldBe` 351901

    it "solves a simple Day 7B example" $ do
      nonLinearFuelToAlignCrabs example `shouldBe` 168

    it "solves Day 7B" $ do
      input <- readCommaSeparatedInts . head <$> readLines "inputs/day7.txt"
      nonLinearFuelToAlignCrabs input `shouldBe` 101079875

  describe "Day 8" $ do
    it "solves a simple Day 8A example" $ do
      example <- map (map words . split (dropDelims $ oneOf "|")) <$> readLines "inputs/day8-example.txt"
      let outputs = concatMap tail example
      sum (map countDigits1478 outputs) `shouldBe` 26

    it "solves Day 8A" $ do
      input <- map (map words . split (dropDelims $ oneOf "|")) <$> readLines "inputs/day8.txt"
      let outputs = concatMap tail input
      sum (map countDigits1478 outputs) `shouldBe` 488

    it "solves a simple Day 8B example" $ do
      example <- map (map words . split (dropDelims $ oneOf "|")) <$> readLines "inputs/day8-example.txt"
      sum (map (\line -> decode (head line) (head (tail line))) example) `shouldBe` 61229

    it "solves Day 8B" $ do
      input <- map (map words . split (dropDelims $ oneOf "|")) <$> readLines "inputs/day8.txt"
      sum (map (\line -> decode (head line) (head (tail line))) input) `shouldBe` 1040429

  describe "Day 9" $ do
    let example =
          listArray (0, 4) $
            map
              (listArray (0, 9))
              [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
                [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
                [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
                [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
                [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
              ]
    it "solves a simple Day 9A example" $ do
      lowPoints example `shouldBe` [(0, 1), (0, 9), (2, 2), (4, 6)]
      sum (map (riskLevel example) (lowPoints example)) `shouldBe` 15

    it "solves Day 9A" $ do
      lines <- readLines "inputs/day9.txt"
      let input = map (map (read . (: []))) lines
      let grid = listArray (0, 99) (map (listArray (0, 99)) input)
      sum (map (riskLevel grid) (lowPoints grid)) `shouldBe` 545

    it "solves a simple Day 9B example" $ do
      basinSizes example `shouldBe` foldr Heap.insert Heap.empty [3, 9, 14, 9]
      (product . Heap.take 3 . basinSizes) example `shouldBe` 1134

    it "solves Day 9B" $ do
      lines <- readLines "inputs/day9.txt"
      let input = map (map (read . (: []))) lines
      let grid = listArray (0, length input - 1) (map (listArray (0, length (head input) - 1)) input)
      (product . Heap.take 3 . basinSizes) grid `shouldBe` 950600

  describe "Day 10" $ do
    it "solves Day 10A" $ do
      input <- readLines "inputs/day10.txt"
      sum (map (scoreSyntax . checkSyntax) input) `shouldBe` 369105
    it "solves a simple Day 10B example" $ do
      let example = "}}]])})]"
      scoreAutocomplete (Incomplete example) `shouldBe` 288957
    it "solves Day 10B" $ do
      input <- readLines "inputs/day10.txt"
      (median . filter (/= 0) . map (scoreAutocomplete . checkSyntax) $ input) `shouldBe` 3999363569

  describe "Day 11" $ do
    let example = [ "start-A",
                    "start-b",
                    "A-c",
                    "A-b",
                    "b-d",
                    "A-end",
                    "b-end" ]

    it "solves a simple Day11A example" $ do
      length (paths (buildGraph example)) `shouldBe` 10

    it "solves Day 11A" $ do
      input <- readLines "inputs/day11.txt"
      length (paths (buildGraph input)) `shouldBe` 4659

    it "solves a simple Day11B example" $ do
      length (pathsDoubleVisit (buildGraph example)) `shouldBe` 36

    it "solves Day 11B" $ do
      input <- readLines "inputs/day11.txt"
      length (pathsDoubleVisit (buildGraph input)) `shouldBe` 148962

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

readInput :: FilePath -> IO [Int]
readInput fp = mapMaybe readMaybe <$> readLines fp

readCommand :: String -> Maybe (String, Int)
readCommand s = do
  case words s of
    [c, n] -> (,) c <$> readMaybe n
    _ -> Nothing

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = mapMaybe readMaybe . split (dropDelims $ oneOf ",")

readWhitespaceSeparatedInts :: String -> [Int]
readWhitespaceSeparatedInts = mapMaybe readMaybe . words

readPoints :: String -> ((Int, Int), (Int, Int))
readPoints = zipTuple . mapMaybe readMaybe . split (dropDelims $ oneOf ", ->")
  where
    zipTuple (x : y : x' : y' : _) = ((x, y), (x', y'))
    zipTuple _ = error "Invalid input"

readBingoBoards :: [[Int]] -> [([[Int]], [[Int]])]
readBingoBoards (_ : a : b : c : d : e : rest) = ([a, b, c, d, e], transpose [a, b, c, d, e]) : readBingoBoards rest
readBingoBoards _ = []
