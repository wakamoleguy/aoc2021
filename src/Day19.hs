module Day19 (part19a, part19b) where
import           Data.List          (group, sort)
import           Data.List.NonEmpty (groupWith)
import           Data.List.Split    (splitWhen)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (catMaybes, listToMaybe)
import           Data.MemoTrie      (memo2)
import qualified Data.Set           as Set
import           Debug.Trace
import           Util               (readCommaSeparatedInts, readLines)
--------------------------------------------------------------------------------
-- Day 19 - Beacon Scanner
--------------------------------------------------------------------------------
type Coord = (Int, Int, Int)
type CoordSet = [Coord]
type Scanner = [CoordSet]
type OrientedScanner = (Coord, CoordSet)

-- Input parsing
input :: IO [String]
input = readLines "inputs/day19.txt"

listToCoord :: [Int] -> Coord
listToCoord [x, y, z] = (x, y, z)
listToCoord _         = error "listToCoord: invalid input"

parsedScannerCoords :: [String] -> [CoordSet]
parsedScannerCoords = map parseSingleScanner . splitWhen (== "")
  where
    parseSingleScanner = map (listToCoord . readCommaSeparatedInts) . tail

-- Generate rotations and perspectives
rotations :: [Coord -> Coord]
rotations = [ \(x, y, z) -> (x, y, z)
            , \(x, y, z) -> (x, -z, y)
            , \(x, y, z) -> (x, -y, -z)
            , \(x, y, z) -> (x, z, -y)
            , \(x, y, z) -> (-x, -y, z)
            , \(x, y, z) -> (-x, -z, -y)
            , \(x, y, z) -> (-x, y, -z)
            , \(x, y, z) -> (-x, z, y) ]

turns :: [Coord -> Coord]
turns = [ \(x, y, z) -> (x, y, z)
        , \(x, y, z) -> (z, x, y)
        , \(x, y, z) -> (y, z, x) ]

perspectives :: [Coord -> Coord]
perspectives = (.) <$> turns <*> rotations

allPerspectives :: CoordSet -> Scanner
allPerspectives cs = fmap <$> perspectives <*> pure cs

diff :: Coord -> Coord -> Coord
diff (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
add :: Coord -> Coord -> Coord
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

orientTo :: Scanner -> OrientedScanner -> Maybe OrientedScanner
orientTo = memo2 orientTo'
  where
    orientTo' unoriented (pos, target) = listToMaybe $ do
      rotated <- unoriented
      let diffMap = Map.fromListWith (+) [(diff r t, 1 :: Int) | r <- rotated, t <- target]
      validDiff <- Map.keys $ Map.filter (>= 12) diffMap
      let shiftedRotated = fmap (`diff` validDiff) rotated
      return (diff (0, 0, 0) validDiff, shiftedRotated)

orientToAny :: Scanner -> [OrientedScanner] -> Maybe OrientedScanner
orientToAny unoriented targets = listToMaybe $ catMaybes $ orientTo unoriented <$> targets

orientAll :: [OrientedScanner] -> [Scanner] -> [OrientedScanner]
orientAll oriented [] = oriented
orientAll oriented (u:us) =
  case orientToAny u oriented of
    Nothing           -> orientAll oriented (us ++ [u]) -- Try later
    Just nextOriented -> orientAll (nextOriented : oriented) us

mappedScanners :: IO [OrientedScanner]
mappedScanners = do
  parsed <- parsedScannerCoords <$> input
  let original = head parsed
  let scanners = map allPerspectives $ tail parsed
  return $ orientAll [((0, 0, 0), original)] scanners

part19a :: IO Int
part19a = length . map head . group . sort. concatMap snd <$> mappedScanners

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

part19b :: IO Int
part19b = do
  scanners <- map fst <$> mappedScanners
  return $ maximum [manhattanDistance p1 p2 | p1 <- scanners, p2 <- scanners, p1 /= p2]
