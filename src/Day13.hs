module Day13 (part13a, part13b) where
import           Control.Applicative (liftA2)
import           Data.Foldable       (foldl')
import           Data.List           (isPrefixOf)
import qualified Data.Set            as Set
import           Util                (readCommaSeparatedInts, readLines)

--------------------------------------------------------------------------------
-- Day 13 - Transparent Origami
--------------------------------------------------------------------------------

data Fold = XF Int | YF Int

input = readLines "inputs/day13.txt"
coords = map ((\[x, y] -> (x, y)) . readCommaSeparatedInts) . takeWhile (/= "") <$> input
folds = map readFold . drop 1 . dropWhile (/= "") <$> input

readFold :: String -> Fold
readFold s
  | "fold along x=" `isPrefixOf` s = XF $ read $ drop 13 s
  | "fold along y=" `isPrefixOf` s = YF $ read $ drop 13 s
  | otherwise = undefined

foldDot :: Fold -> (Int, Int) -> (Int, Int)
foldDot (XF axis) (x, y)
  | x < axis = (x, y)
  | otherwise = (2 * axis - x, y)
foldDot (YF axis) (x, y)
  | y < axis = (x, y)
  | otherwise = (x, 2 * axis - y)

part13a = do
  folder <- foldDot . head <$> folds
  Set.size . Set.fromList . fmap folder <$> coords
part13b = do
  endDots <- Set.fromList <$> liftA2 (foldl' (\c f -> fmap (foldDot f) c)) coords folds
  let grid = [[if Set.member (x, y) endDots then '#' else ' ' | x <- [0 .. 40]] | y <- [0 .. 5]]
  return $ unlines grid
