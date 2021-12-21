module Day21 (part21a, part21b) where
import           Control.Monad.State (State, evalState, get, put, runState)
import           Data.Foldable       (foldl')
import           Data.List           (foldl1')
import           Data.MemoTrie       (memo)
import           Data.Tuple          (swap)
import           Debug.Trace

--------------------------------------------------------------------------------
-- Day 21 - Dirac Dice
--------------------------------------------------------------------------------

startingPosition :: (Int, Int)
startingPosition = (4, 6)

data Player = P { pos :: Int, score :: Int } deriving (Eq, Show)
data GameState = GS { players :: [Player], rolls :: Int } deriving (Eq, Show)

die :: [Int]
die = cycle [1..100]

rollDie :: State [Int] Int
rollDie = do
  d <- get
  put (tail d)
  return (head d)

turn :: GameState -> State [Int] GameState
turn GS { players = (P { pos = pos, score = score }:ps), rolls = rolls} = do
  r1 <- rollDie
  r2 <- rollDie
  r3 <- rollDie
  let newPos = ((pos + r1 + r2 + r3 - 1) `mod` 10) + 1
  let newScore = score + newPos
  let newPlayer = P { pos = newPos, score = newScore }
  return $ GS { players = ps ++ [newPlayer], rolls = rolls + 3 }
turn gs = pure gs

playGame :: GameState -> State [Int] GameState
playGame gs@GS { players = ps, rolls = r } =
  if any (\p -> score p >= 1000) ps then
    pure gs
  else
    turn gs >>= playGame

checkSum :: GameState -> Int
checkSum GS { players = ps, rolls = r} =
  let loser = minimum $ map score ps
  in r * loser

part21a :: IO Int
part21a = pure $ checkSum $ flip evalState die $ playGame (GS
          { players = [ P { pos = fst startingPosition, score = 0 }
                      , P { pos = snd startingPosition, score = 0 } ]
          , rolls = 0 })


--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------
-- 1x3, 3x4, 6x5, 7x6, 6x7, 3x8, 1x9
move :: (Int, Int) -> Int -> (Int, Int)
move (pos, score) steps =
  let newPos = ((pos + steps - 1) `mod` 10) + 1
  in (newPos, score + newPos)

addPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair (a, b) (c, d) = (a + c, b + d)
timesPair :: Int -> (Int, Int) -> (Int, Int)
timesPair n (a, b) = (n * a, n * b)

countWins :: ((Int, Int), (Int, Int)) -> (Int, Int)
countWins (p1, p2)
  | snd p1 >= 21 = (0, 0)
  | snd p2 >= 21 = (0, 1)
  | otherwise =
    let rolls = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
    in swap $ foldl1' addPair [ timesPair f (countWinsMemo (p2, move p1 r)) | (r, f) <- rolls ]

countWinsMemo :: ((Int, Int), (Int, Int)) -> (Int, Int)
countWinsMemo = memo countWins

part21b :: IO Int
part21b = pure $ (\(a, b) -> maximum [a, b]) $ countWinsMemo
  ( (fst startingPosition, 0)
  , (snd startingPosition, 0) )
