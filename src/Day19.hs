module Day19 (part19a, part19b) where
import           Data.List.Split (splitWhen)
import           Data.Maybe      (catMaybes, listToMaybe)
import qualified Data.Set        as Set
import           Util            (readCommaSeparatedInts, readLines)
--------------------------------------------------------------------------------
-- Day 19 - Beacon Scanner
--------------------------------------------------------------------------------
type Coord = (Int, Int, Int)
type CoordSet = Set.Set Coord

-- Input parsing
input :: IO [String]
input = readLines "inputs/day19.txt"

listToCoord :: [Int] -> Coord
listToCoord [x, y, z] = (x, y, z)
listToCoord _         = error "listToCoord: invalid input"

parsedScannerCoords :: [String] -> [CoordSet]
parsedScannerCoords = map parseSingleScanner . splitWhen (== "")
  where
    parseSingleScanner = Set.fromList . map (listToCoord . readCommaSeparatedInts) . tail

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

-- Rotate the first set until it overlaps with the second, then return the result
-- If not overlap is found, return Nothing
orientTo :: CoordSet -> ((Int, Int, Int), CoordSet) -> Maybe ((Int, Int, Int), CoordSet)
orientTo unoriented ((dtx, dty, dtz), target) = listToMaybe $ catMaybes $ do
  perspective <- perspectives
  let rotated = Set.map perspective unoriented
  (tdx, tdy, tdz) <- Set.toList target
  (rdx, rdy, rdz) <- Set.toList rotated
  let (dx, dy, dz) = (rdx - tdx, rdy - tdy, rdz - tdz)
  let shiftedRotated = Set.map (\(x, y, z) -> (x - dx, y - dy, z - dz)) rotated
  let sizeOfIntersection = Set.size $ Set.intersection shiftedRotated target
  if sizeOfIntersection >= 12
    then return (Just ((-dx, -dy, -dz), shiftedRotated))
    else return Nothing

orientToAny :: CoordSet -> [((Int, Int, Int), CoordSet)] -> Maybe ((Int, Int, Int), CoordSet)
orientToAny unoriented targets = listToMaybe $ catMaybes $ orientTo unoriented <$> targets

orientAll :: [((Int, Int, Int), CoordSet)] -> [CoordSet] -> [((Int, Int, Int), CoordSet)]
orientAll oriented [] = oriented
orientAll oriented unoriented =
  let next = head unoriented
  in case orientToAny next oriented of
    Nothing           -> orientAll oriented (tail unoriented ++ [next]) -- Try later
    Just nextOriented -> orientAll (nextOriented : oriented) (tail unoriented)


mappedScanners :: IO [((Int, Int, Int), CoordSet)]
mappedScanners = do
  scanners <- parsedScannerCoords <$> input
  return $ orientAll [((0, 0, 0), head scanners)] (tail scanners)

part19a :: IO Int
part19a = Set.size . Set.unions . map snd <$> mappedScanners

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

part19b :: IO Int
part19b = do
  scanners <- map fst <$> mappedScanners
  return $ maximum [manhattanDistance p1 p2 | p1 <- scanners, p2 <- scanners, p1 /= p2]
