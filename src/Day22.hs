module Day22 (part22a, part22b) where
import           Data.List   (foldl', foldl1')
import           Data.Maybe  (fromMaybe, listToMaybe, mapMaybe)
import           Debug.Trace
import           Text.Parsec (Parsec, char, many1, oneOf, parse, string, (<|>))

--------------------------------------------------------------------------------
-- Day 22 - Reactor Reboot
--------------------------------------------------------------------------------

type Bound = (Int, Int)
data Cube = Cube Bool Bound Bound Bound deriving (Eq, Show)

-- Parse Input
-- on x=-48..6,y=-13..40,z=-12..35
parseBit :: Parsec String a Bool
parseBit = do
  string "o"
  bit <- string "n " <|> string "ff "
  return $ bit == "n "

parseBound :: Parsec String a Bound
parseBound = do
  oneOf "xyz"
  char '='
  min <- read <$> many1 (oneOf "-0123456789")
  string ".."
  max <- read <$> many1 (oneOf "-0123456789")
  return (min, max+1)

parseCube :: Parsec String a Cube
parseCube = do
  bit <- parseBit
  x <- parseBound
  string ","
  y <- parseBound
  string ","
  z <- parseBound
  char '\n'
  return $ Cube bit x y z

parseInput :: IO [Cube]
parseInput = do
  input <- readFile "inputs/day22.txt"
  return $ case parse (many1 parseCube) "" input of
    Left err    -> error $ show err
    Right cubes -> cubes


inRange :: Bound -> Int -> Bool
inRange (xmin, xmax) x = xmin <= x && x < xmax

-- Part 1

initializationRegion :: Bound
initializationRegion = (-50, 51)

initializationCubes :: [Cube] -> [Cube]
initializationCubes = filter isInInitializationRegion
  where
    isInInitializationRegion (Cube _ (minx, maxx) (miny, maxy) (minz, maxz)) =
      all (inRange initializationRegion) [minx, maxx, miny, maxy, minz, maxz]

maybeSet :: (Int, Int, Int) -> Cube -> Maybe Bool
maybeSet (x, y, z) (Cube b xr yr zr) = if inRange xr x && inRange yr y && inRange zr z
  then Just b
  else Nothing

setBit :: [Cube] -> (Int, Int, Int) -> Bool
setBit cubes p = fromMaybe False $ listToMaybe $ mapMaybe (maybeSet p) rcubes
  where rcubes = reverse cubes

part22a :: IO Int
part22a = do
  cubes <- initializationCubes <$> parseInput
  let overlaid = foldl' go [] cubes
  return $ sum $ map volume $ filter isOn overlaid

-- Part 2
isEntirelyWithin :: Cube -> Cube -> Bool
isEntirelyWithin (Cube _ inX inY inZ) (Cube _ outX outY outZ) =
  fst outX <= fst inX && snd inX <= snd outX &&
  fst outY <= fst inY && snd inY <= snd outY &&
  fst outZ <= fst inZ && snd inZ <= snd outZ

isDisjointFrom :: Cube -> Cube -> Bool
isDisjointFrom (Cube _ inX inY inZ) (Cube _ outX outY outZ) =
  -- Shifted enough in any of the 6 directions
  (fst inX >= snd outX) || (fst outX >= snd inX) ||
  (fst inY >= snd outY) || (fst outY >= snd inY) ||
  (fst inZ >= snd outZ) || (fst outZ >= snd inZ)

volume :: Cube -> Int
volume (Cube _ (x0, x1) (y0, y1) (z0, z1)) = max 0 (x1 - x0) * max 0 (y1 - y0) * max 0 (z1 - z0)

overlayCuboid :: Cube -> Cube -> [Cube]
overlayCuboid top bottom
  | bottom `isEntirelyWithin` top = [] -- Top masks bottom
  | bottom `isDisjointFrom` top = [bottom] -- Bottom is unaffected by top
  | otherwise =
    let (Cube b0 (x0, x1) (y0, y1) (z0, z1)) = bottom
        (Cube _ (x0', x1') (y0', y1') (z0', z1')) = top
        (xmin, xmax) = (max x0 x0', min x1 x1')
        (ymin, ymax) = (max y0 y0', min y1 y1')
        (zmin, zmax) = (max z0 z0', min z1 z1')
        a = Cube b0 (x0, x1) (ymax, y1) (z0, z1) -- left face
        b = Cube b0 (x0, x1) (y0, ymin) (z0, z1) -- right face
        c = Cube b0 (x0, x1) (ymin, ymax) (z0, zmin) -- bottom insert
        d = Cube b0 (x0, x1) (ymin, ymax) (zmax, z1) -- top insert
        e = Cube b0 (x0, xmin) (ymin, ymax) (zmin, zmax) -- front plug
        f = Cube b0 (xmax, x1) (ymin, ymax) (zmin, zmax) -- back plug
        allCuboids = [a, b, c, d, e, f]
    in filter ((> 0) . volume) allCuboids

go :: [Cube] -> Cube -> [Cube]
go cubes top = ([top | isOn top]) ++ concatMap (overlayCuboid top) cubes

isOn :: Cube -> Bool
isOn (Cube b _ _ _) = b

part22b :: IO Int
part22b = do
  cubes <- parseInput
  let overlaid = foldl' go [] cubes
  return $ sum $ map volume $ filter isOn overlaid
