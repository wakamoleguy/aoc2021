import Control.Monad
import Data.Maybe
import Lib
import Test.Hspec
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
    it "solves a simple Day 2A example" $ do
      let input =
            [ ("forward", 5),
              ("down", 5),
              ("forward", 8),
              ("up", 3),
              ("down", 8),
              ("forward", 2)
            ]
      uncurry (*) (calculateCoordinates input) `shouldBe` 150

    it "solves Day2A" $ do
      expected <- calculateCoordinates . mapMaybe readCommand <$> readLines "inputs/day2.txt"
      uncurry (*) expected `shouldBe` 1804520

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

readInput :: FilePath -> IO [Int]
readInput fp = mapMaybe readMaybe <$> readLines fp

readCommand :: String -> Maybe (String, Int)
readCommand s = do
  case words s of
    [c, n] -> (,) c <$> readMaybe n
    _ -> Nothing
