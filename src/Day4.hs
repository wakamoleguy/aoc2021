module Day4 (part4a, part4b) where
import           Data.Foldable (find)
import           Data.List     (transpose)
import           Util          (readCommaSeparatedInts, readLines,
                                readWhitespaceSeparatedInts)

--------------------------------------------------------------------------------
-- Day 4 - Giant Squid
--------------------------------------------------------------------------------

-- We store the transposed board for convenience
type BingoBoard = ([[Int]], [[Int]])

input :: IO [String]
input = readLines "inputs/day4.txt"

readBingoBoards :: [[Int]] -> [BingoBoard]
readBingoBoards (_ : a : b : c : d : e : rest) = ([a, b, c, d, e], transpose [a, b, c, d, e]) : readBingoBoards rest
readBingoBoards _ = []

bingoCall :: IO [Int]
bingoCall = readCommaSeparatedInts . head <$> input
bingoBoards :: IO [([[Int]], [[Int]])]
bingoBoards = readBingoBoards . map readWhitespaceSeparatedInts . tail <$> input

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

part4a :: IO Int
part4a = playBingo <$> bingoCall <*> bingoBoards

part4b :: IO Int
part4b = loseAtBingo <$> bingoCall <*> bingoBoards
