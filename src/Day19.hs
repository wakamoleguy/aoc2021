module Day19 (part19a, part19b) where

--------------------------------------------------------------------------------
-- Day 19 - Beacon Scanner
--------------------------------------------------------------------------------
type Coord = (Int, Int, Int)

rotations :: Coord -> [Coord]
rotations (x, y, z) = [ (x, y, z), (x, -z, y), (x, -y, -z), (x, z, -y)
                      , (-x, -y, z), (-x, -z, -y), (-x, y, -z), (-x, z, y)]
turns :: Coord -> [Coord]
turns (x, y, z) = [ (x, y, z), (z, x, y), (y, z, x) ]

perspectives :: Coord -> [Coord]
perspectives c = pure c >>= rotations >>= turns

part19a :: IO String
part19a = pure $ show $ perspectives (1, 2, 3)

part19b :: IO String
part19b = pure "World"
