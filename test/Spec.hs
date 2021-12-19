import           Day1       (part1a, part1b)
import           Day10      (part10a, part10b)
import           Day11      (part11a, part11b)
import           Day12      (part12a, part12b)
import           Day13      (part13a, part13b)
import           Day14      (part14a, part14b)
import           Day15      (part15a, part15b)
import           Day16      (part16a, part16b)
import           Day17      (part17a, part17b)
import           Day18      (part18a, part18b)
import           Day19      (part19a, part19b)
import           Day2       (part2a, part2b)
import           Day3       (part3a, part3b)
import           Day4       (part4a, part4b)
import           Day5       (part5a, part5b)
import           Day6       (part6a, part6b)
import           Day7       (part7a, part7b)
import           Day8       (part8a, part8b)
import           Day9       (part9a, part9b)
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day 19" $ do
    it "solves day19a" $ do
      part19a >>= (`shouldBe` 376)
    it "solves day19b" $ do
      part19b >>= (`shouldBe` 10772)

  -- describe "Day 18" $ do
  --   it "solves day18a" $ do
  --     part18a >>= (`shouldBe` 2501)
  --   it "solves day18b" $ do
  --     part18b >>= (`shouldBe` 4935)

  -- describe "Day 17" $ do
  --   it "solves day17a" $ do
  --     part17a >>= (`shouldBe` 5778)
  --   it "solves day17b" $ do
  --     part17b >>= (`shouldBe` 2576)

  -- describe "Day 16" $ do
  --   it "solves day16a" $ do
  --     part16a >>= (`shouldBe` 866)
  --   it "solves day16b" $ do
  --     part16b >>= (`shouldBe` 1392637195518)

  describe "Day 1" $ do
    it "solves Day1A" $ do
      part1a >>= (`shouldBe` 1215)
    it "solves Day1B" $ do
      part1b >>= (`shouldBe` 1150)

  -- describe "Day 2" $ do
  --   it "solves Day2A" $ do
  --     part2a >>= (`shouldBe` 1804520)
  --   it "solves Day2B" $ do
  --     part2b >>= (`shouldBe` 1971095320)

  -- describe "Day 3" $ do
  --   it "solves Day3A" $ do
  --     part3a >>= (`shouldBe` 2648450)
  --   it "solves Day3B" $ do
  --     part3b >>= (`shouldBe` 2845944)

  -- describe "Day 4" $ do
  --   it "solves Day4a" $ do
  --     part4a >>= (`shouldBe` 8136)

  --   it "solves Day4b" $ do
  --     part4b >>= (`shouldBe` 12738)

  -- describe "Day 5" $ do
  --   it "solves day5a" $ do
  --     part5a >>= (`shouldBe` 6564)
  --   it "solves day5b" $ do
  --     part5b >>= (`shouldBe` 19172)

  -- describe "Day 6" $ do
  --   it "solves day6a" $ do
  --     part6a >>= (`shouldBe` 371379)
  --   it "solves day6b" $ do
  --     part6b >>= (`shouldBe` 1674303997472)

  -- describe "Day 7" $ do
  --   it "solves day7a" $ do
  --     part7a >>= (`shouldBe` 351901)
  --   it "solves day7b" $ do
  --     part7b >>= (`shouldBe` 101079875)

  -- describe "Day 8" $ do
  --   it "solves day8a" $ do
  --     part8a >>= (`shouldBe` 488)
  --   it "solves day8b" $ do
  --     part8b >>= (`shouldBe` 1040429)

  -- describe "Day 9" $ do
  --   it "solves day9a" $ do
  --     part9a >>= (`shouldBe` 545)
  --   it "solves day9b" $ do
  --     part9b >>= (`shouldBe` 950600)

  -- describe "Day 10" $ do
  --   it "solves day10a" $ do
  --     part10a >>= (`shouldBe` 369105)
  --   it "solves day10b" $ do
  --     part10b >>= (`shouldBe` 3999363569)

  -- describe "Day 11" $ do
  --   it "solves day11a" $ do
  --     part11a >>= (`shouldBe` 1688)
  --   it "solves day11b" $ do
  --     part11b >>= (`shouldBe` 403)

  -- describe "Day 12" $ do
  --   it "solves day12a" $ do
  --     part12a >>= (`shouldBe` 4659)
  --   it "solves day12b" $ do
  --     part12b >>= (`shouldBe` 148962)

  -- describe "Day 13" $ do
  --   it "solves day13a" $ do
  --     part13a >>= (`shouldBe` 653)

  --   it "solves day13b" $ do
  --     part13b >>= putStrLn

  -- describe "Day 14" $ do
  --   it "solves day14a" $ do
  --     part14a >>= (`shouldBe` 2768)
  --   it "solves day14b" $ do
  --     part14b >>= (`shouldBe` 2914365137499)
  -- describe "Day 15" $ do
  --   it "solves day15a" $ do
  --     part15a >>= (`shouldBe` 702)
  --   it "solves day15b" $ do
  --     part15b >>= (`shouldBe` 2955)
