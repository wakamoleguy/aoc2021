module Day17 (part17a, part17b) where
import           Data.Array (inRange)

--------------------------------------------------------------------------------
-- Day 17 - Trick Shot
--------------------------------------------------------------------------------

example :: ((Int, Int), (Int, Int))
example = ((20, 30), (-10, -5))

input :: ((Int, Int), (Int, Int))
input = ((169, 206), (-108, -68))

xSpeeds :: (Int, Int) -> [Int]
xSpeeds xRange@(xMin, xMax) = filter (speedLandsInRange 0) [1..xMax]
  where
    speedLandsInRange pos vel
      | inRange xRange pos = True
      | pos > xMax || vel == 0 = False
      | otherwise = speedLandsInRange (pos + vel) (max 0 (vel-1))

ySpeeds :: Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
ySpeeds xSpeed ((minX, maxX), (minY, maxY)) = filter (landsInRange (0, 0)) [(xSpeed, y) | y <- [-biggestPossibleY..biggestPossibleY]]
  where
    biggestPossibleY = 2 - minY
    landsInRange (posX, posY) (velX, velY)
      | inRange (minX, maxX) posX && inRange (minY, maxY) posY = True
      | posY < minY = False -- Fell too far
      | posX > maxX = False -- Overshot
      | otherwise = landsInRange (posX + velX, posY + velY) (max 0 (velX-1), velY-1)

highestPoint :: (Int, Int) -> Int
highestPoint (_, y)
  | y < 0 = y
  | otherwise = y * (y + 1) `div` 2

part17a :: IO Int
part17a = pure $ maximum $ map highestPoint $ concatMap (`ySpeeds` input) $ xSpeeds (fst input)

part17b :: IO Int
part17b = pure $ length $ concatMap (`ySpeeds` input) $ xSpeeds (fst input)
