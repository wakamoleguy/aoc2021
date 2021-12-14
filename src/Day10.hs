
module Day10 (part10a, part10b) where
import           Data.List (foldl', sort)
import           Util      (readLines)

--------------------------------------------------------------------------------
-- Day 10 - Syntax Scoring
--------------------------------------------------------------------------------
input = readLines "inputs/day10.txt"

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

part10a = sum . map (scoreSyntax . checkSyntax) <$> input
part10b = median . filter (/= 0) . map (scoreAutocomplete . checkSyntax) <$> input
