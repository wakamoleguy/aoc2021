import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import Lib
import Test.Hspec
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

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

readInput :: FilePath -> IO [Int]
readInput fp = mapMaybe readMaybe <$> readLines fp

readCommand :: String -> Maybe (String, Int)
readCommand s = do
  case words s of
    [c, n] -> (,) c <$> readMaybe n
    _ -> Nothing
