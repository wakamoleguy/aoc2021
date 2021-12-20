module Util (readLines, readCommaSeparatedInts, readWhitespaceSeparatedInts, listToArray) where
import           Data.Array      (Array, listArray)
import           Data.List.Split (split, splitOn)
import           Data.Maybe      (mapMaybe)
import           Text.Read       (readMaybe)

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = mapMaybe readMaybe . splitOn ","

readWhitespaceSeparatedInts :: String -> [Int]
readWhitespaceSeparatedInts = mapMaybe readMaybe . words

listToArray :: [a] -> Array Int a
listToArray a = listArray (0, length a - 1) a
