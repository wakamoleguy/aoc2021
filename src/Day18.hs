module Day18 (part18a, part18b) where
import           Data.Either                (fromRight, rights)
import           Data.Functor.Contravariant (Op (getOp))
import           Data.List                  (foldl1')
import           Debug.Trace
import           Text.Parsec                (Parsec, char, digit, parse, tab,
                                             (<|>))

--------------------------------------------------------------------------------
-- Day 18 - Snailfish
--------------------------------------------------------------------------------

data Tree = Leaf Int | Node Tree Tree deriving (Show, Eq)

literalParser :: Parsec String a Tree
literalParser = do
  n <- digit
  return $ Leaf (read [n])

treeParser :: Parsec String a Tree
treeParser = literalParser <|> do
  char '['
  left <- treeParser
  char ','
  right <- treeParser
  char ']'
  return $ Node left right

addToLeftmost :: Int -> Tree -> Tree
addToLeftmost a (Leaf b)          = Leaf (a + b)
addToLeftmost a (Node left right) = Node (addToLeftmost a left) right

addToRightmost :: Int -> Tree -> Tree
addToRightmost a (Leaf b)          = Leaf (a + b)
addToRightmost a (Node left right) = Node left (addToRightmost a right)

explode :: Tree -> (Bool, Tree)
explode (Node (Node (Node (Node (Node (Leaf a) (Leaf b)) r1) r2) r3) r4) =
  (True, Node (Node (Node (Node (Leaf 0) (addToLeftmost b r1)) r2) r3) r4)
explode (Node (Node (Node (Node l1 (Node (Leaf a) (Leaf b))) r1) r2) r3) =
  (True, Node (Node (Node (Node (addToRightmost a l1) (Leaf 0)) (addToLeftmost b r1)) r2) r3)
explode (Node (Node (Node l1 (Node (Node (Leaf a) (Leaf b)) r1)) r2) r3) =
  (True, Node (Node (Node (addToRightmost a l1) (Node (Leaf 0) (addToLeftmost b r1))) r2) r3)
explode (Node (Node (Node l1 (Node l2 (Node (Leaf a) (Leaf b)))) r1) r2) =
  (True, Node (Node (Node l1 (Node (addToRightmost a l2) (Leaf 0))) (addToLeftmost b r1)) r2)
explode (Node (Node l1 (Node (Node (Node (Leaf a) (Leaf b)) r1) r2)) r3) =
  (True, Node (Node (addToRightmost a l1) (Node (Node (Leaf 0) (addToLeftmost b r1)) r2)) r3)
explode (Node (Node l1 (Node (Node l2 (Node (Leaf a) (Leaf b))) r1)) r2) =
  (True, Node (Node l1 (Node (Node (addToRightmost a l2) (Leaf 0)) (addToLeftmost b r1))) r2)
explode (Node (Node l1 (Node l2 (Node (Node (Leaf a) (Leaf b)) r1))) r2) =
  (True, Node (Node l1 (Node (addToRightmost a l2) (Node (Leaf 0) (addToLeftmost b r1)))) r2)
explode (Node (Node l1 (Node l2 (Node l3 (Node (Leaf a) (Leaf b))))) r1) =
  (True, Node (Node l1 (Node l2 (Node (addToRightmost a l3) (Leaf 0)))) (addToLeftmost b r1))
explode (Node l1 (Node (Node (Node (Node (Leaf a) (Leaf b)) r1) r2) r3)) =
  (True, Node (addToRightmost a l1) (Node (Node (Node (Leaf 0) (addToLeftmost b r1)) r2) r3))
explode (Node l1 (Node (Node (Node l2 (Node (Leaf a) (Leaf b))) r1) r2)) =
  (True, Node l1 (Node (Node (Node (addToRightmost a l2) (Leaf 0)) (addToLeftmost b r1)) r2))
explode (Node l1 (Node (Node l2 (Node (Node (Leaf a) (Leaf b)) r1)) r2)) =
  (True, Node l1 (Node (Node (addToRightmost a l2) (Node (Leaf 0) (addToLeftmost b r1))) r2))
explode (Node l1 (Node (Node l2 (Node l3 (Node (Leaf a) (Leaf b)))) r1)) =
  (True, Node l1 (Node (Node l2 (Node (addToRightmost a l3) (Leaf 0))) (addToLeftmost b r1)))
explode (Node l1 (Node l2 (Node (Node (Node (Leaf a) (Leaf b)) r1) r2))) =
  (True, Node l1 (Node (addToRightmost a l2) (Node (Node (Leaf 0) (addToLeftmost b r1)) r2)))
explode (Node l1 (Node l2 (Node (Node l3 (Node (Leaf a) (Leaf b))) r1))) =
  (True, Node l1 (Node l2 (Node (Node (addToRightmost a l3) (Leaf 0)) (addToLeftmost b r1))))
explode (Node l1 (Node l2 (Node l3 (Node (Node (Leaf a) (Leaf b)) r1)))) =
  (True, Node l1 (Node l2 (Node (addToRightmost a l3) (Node (Leaf 0) (addToLeftmost b r1)))))
explode (Node l1 (Node l2 (Node l3 (Node l4 (Node (Leaf a) (Leaf b)))))) =
  (True, Node l1 (Node l2 (Node l3 (Node (addToRightmost a l4) (Leaf 0)))))
explode t = (False, t)

splitOne :: Tree -> (Bool, Tree)
splitOne = go False
  where go True t = (True, t)
        go False (Leaf x)
          | x >= 10 = (True, Node (Leaf (x `div` 2)) (Leaf ((x+1) `div` 2)))
          | otherwise = (False, Leaf x)
        go False (Node l r) =
          let (lsplit, l') = go False l
              (rsplit, r') = go False r
          in if lsplit then (lsplit, Node l' r) else if rsplit then (rsplit, Node l r') else (False, Node l r)

reduce :: Tree -> Tree
reduce tree =
  let (exploded, etree) = explode tree
      (split, stree) = splitOne etree
  in if exploded then reduce etree else if split then reduce stree else tree
add :: Tree -> Tree -> Tree
add = Node

magnitude :: Tree -> Int
magnitude (Leaf x)   = x
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r

trees :: IO [Tree]
trees = do
  input <- readFile "inputs/day18.txt"
  return $ rights $ map (parse treeParser "") $ lines input

part18a :: IO Int
part18a = magnitude . foldl1' (\a b -> reduce (add a b)) <$> trees


part18b :: IO Int
part18b = do
  ts <- trees
  return $ maximum $ [magnitude (reduce (add a b)) | a <- ts, b <- ts, a /= b]

