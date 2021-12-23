module Day23 (part23a, part23b) where
import           Data.Graph.AStar (aStar)
import qualified Data.HashSet     as HashSet
import           Data.Hashable    (Hashable, hash, hashWithSalt)
import qualified Data.Heap        as Heap
import           Data.List        (sortOn)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Set         as Set
import           Data.Tuple
import           Debug.Trace

data Burrow = H1 | H2 | H3 | H4 | H5 | H6 | H7
  | A1 | A2 | B1 | B2 | C1 | C2 | D1 | D2
  deriving (Eq, Show, Ord, Enum, Bounded)
instance Hashable Burrow where
  hash = fromEnum
  hashWithSalt salt = hashWithSalt salt . fromEnum

data Amphipod = Amber | Bronze | Copper | Desert deriving (Eq, Show, Ord, Enum)
instance Hashable Amphipod where
  hash = fromEnum
  hashWithSalt salt = hashWithSalt salt . fromEnum

data Game = G {
  h1    :: Maybe Amphipod,
  h2    :: Maybe Amphipod,
  h3    :: Maybe Amphipod,
  h4    :: Maybe Amphipod,
  h5    :: Maybe Amphipod,
  h6    :: Maybe Amphipod,
  h7    :: Maybe Amphipod,
  rooma :: [Amphipod],
  roomb :: [Amphipod],
  roomc :: [Amphipod],
  roomd :: [Amphipod]
} deriving (Eq, Show, Ord)

example :: Game
example = G
  { h1 = Nothing
  , h2 = Nothing
  , h3 = Nothing
  , h4 = Nothing
  , h5 = Nothing
  , h6 = Nothing
  , h7 = Nothing
  , rooma = [Bronze, Amber]
  , roomb = [Copper, Desert]
  , roomc = [Bronze, Copper]
  , roomd = [Desert, Amber] }

input :: Game
input = G
  { h1 = Nothing
  , h2 = Nothing
  , h3 = Nothing
  , h4 = Nothing
  , h5 = Nothing
  , h6 = Nothing
  , h7 = Nothing
  , rooma = [Desert, Copper]
  , roomb = [Bronze, Copper]
  , roomc = [Bronze, Desert]
  , roomd = [Amber, Amber] }

amphipodWeight :: Amphipod -> Int
amphipodWeight Amber  = 1
amphipodWeight Bronze = 10
amphipodWeight Copper = 100
amphipodWeight Desert = 1000

moveFromA :: Game -> [(Game, Int)]
moveFromA g =
  let
    as = rooma g
    anyNotHome = any (/= Amber) as
    (a:rest) = as
    gone = g{rooma = rest}
    moves = catMaybes [
      if isNothing (h1 g) && isNothing (h2 g) then Just (gone{h1 = Just a}, 5 - length as) else Nothing,
      if isNothing (h2 g) then Just (gone{h2 = Just a}, 4 - length as) else Nothing,
      if isNothing (h3 g) then Just (gone{h3 = Just a}, 4 - length as) else Nothing,
      if isNothing (h3 g) && isNothing (h4 g) then Just (gone{h4 = Just a}, 6 - length as) else Nothing,
      if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) then Just (gone{h5 = Just a}, 8 - length as) else Nothing,
      if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) && isNothing (h6 g) then Just (gone{h6 = Just a}, 10 - length as) else Nothing,
      if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) && isNothing (h6 g) && isNothing (h7 g) then Just (gone{h7 = Just a}, 11 - length as) else Nothing ]
  in if anyNotHome then fmap (fmap (* amphipodWeight a)) moves else []

moveFromB :: Game -> [(Game, Int)]
moveFromB g =
  let
    as = roomb g
    anyNotHome = any (/= Bronze) as
    (a:rest) = as
    gone = g{roomb = rest}
    moves = catMaybes [
      if isNothing (h1 g) && isNothing (h2 g) && isNothing (h3 g) then Just (gone{h1 = Just a}, 7 - length as) else Nothing,
      if isNothing (h2 g) && isNothing (h3 g) then Just (gone{h2 = Just a}, 6 - length as) else Nothing,
      if isNothing (h3 g) then Just (gone{h3 = Just a}, 4 - length as) else Nothing,
      if isNothing (h4 g) then Just (gone{h4 = Just a}, 4 - length as) else Nothing,
      if isNothing (h4 g) && isNothing (h5 g) then Just (gone{h5 = Just a}, 6 - length as) else Nothing,
      if isNothing (h4 g) && isNothing (h5 g) && isNothing (h6 g) then Just (gone{h6 = Just a}, 8 - length as) else Nothing,
      if isNothing (h4 g) && isNothing (h5 g) && isNothing (h6 g) && isNothing (h7 g) then Just (gone{h7 = Just a}, 9 - length as) else Nothing ]
  in if anyNotHome then fmap (fmap (* amphipodWeight a)) moves else []

moveFromC :: Game -> [(Game, Int)]
moveFromC g =
  let
    as = roomc g
    anyNotHome = any (/= Copper) as
    (a:rest) = as
    gone = g{roomc = rest}
    moves = catMaybes [
      if isNothing (h1 g) && isNothing (h2 g) && isNothing (h3 g) && isNothing (h4 g) then Just (gone{h1 = Just a}, 9 - length as) else Nothing,
      if isNothing (h2 g) && isNothing (h3 g) && isNothing (h4 g) then Just (gone{h2 = Just a}, 8 - length as) else Nothing,
      if isNothing (h3 g) && isNothing (h4 g) then Just (gone{h3 = Just a}, 6 - length as) else Nothing,
      if isNothing (h4 g) then Just (gone{h4 = Just a}, 4 - length as) else Nothing,
      if isNothing (h5 g) then Just (gone{h5 = Just a}, 4 - length as) else Nothing,
      if isNothing (h5 g) && isNothing (h6 g) then Just (gone{h6 = Just a}, 6 - length as) else Nothing,
      if isNothing (h5 g) && isNothing (h6 g) && isNothing (h7 g) then Just (gone{h7 = Just a}, 7 - length as) else Nothing ]
  in if anyNotHome then fmap (fmap (* amphipodWeight a)) moves else []

moveFromD :: Game -> [(Game, Int)]
moveFromD g =
  let
    as = roomd g
    anyNotHome = any (/= Desert) as
    (a:rest) = as
    gone = g{roomd = rest}
    moves = catMaybes [
      if isNothing (h1 g) && isNothing (h2 g) && isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) then Just (gone{h1 = Just a}, 11 - length as) else Nothing,
      if isNothing (h2 g) && isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) then Just (gone{h2 = Just a}, 10 - length as) else Nothing,
      if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g)  then Just (gone{h3 = Just a}, 8 - length as) else Nothing,
      if isNothing (h4 g) && isNothing (h5 g)  then Just (gone{h4 = Just a}, 6 - length as) else Nothing,
      if isNothing (h5 g) then Just (gone{h5 = Just a}, 4 - length as) else Nothing,
      if isNothing (h6 g) then Just (gone{h6 = Just a}, 4 - length as) else Nothing,
      if isNothing (h6 g) && isNothing (h7 g) then Just (gone{h7 = Just a}, 5 - length as) else Nothing ]
  in if anyNotHome then fmap (fmap (* amphipodWeight a)) moves else []

outMoves :: Game -> [(Game, Int)]
outMoves g = moveFromA g ++ moveFromB g ++ moveFromC g ++ moveFromD g

moveFrom1 :: Game -> Maybe (Game, Int)
moveFrom1 g@G{h1=Just a, h2=Nothing} =
  let gone = g{h1=Nothing}
  in case a of
    Amber -> if all (== Amber) (rooma g) then Just (gone{rooma=Amber:rooma g}, 4 - length (rooma g)) else Nothing
    Bronze -> if isNothing (h3 g) && all (== Bronze) (roomb g) then Just (gone{roomb=Bronze:roomb g}, 10 * (6 - length (roomb g))) else Nothing
    Copper -> if isNothing (h3 g) && isNothing (h4 g) && all (== Copper) (roomc g) then Just (gone{roomc=Copper:roomc g}, 100 * (8 - length (roomc g))) else Nothing
    Desert -> if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) && all (== Desert) (roomd g) then Just (gone{roomd=Desert:roomd g}, 1000 * (10 - length (roomd g))) else Nothing
moveFrom1 _ = Nothing

moveFrom2 :: Game -> Maybe (Game, Int)
moveFrom2 g@G{h2=Just a} =
  let gone = g{h2=Nothing}
  in case a of
    Amber -> if all (== Amber) (rooma g) then Just (gone{rooma=Amber:rooma g}, 3 - length (rooma g)) else Nothing
    Bronze -> if isNothing (h3 g) && all (== Bronze) (roomb g) then Just (gone{roomb=Bronze:roomb g}, 10 * (5 - length (roomb g))) else Nothing
    Copper -> if isNothing (h3 g) && isNothing (h4 g) && all (== Copper) (roomc g) then Just (gone{roomc=Copper:roomc g}, 100 * (7 - length (roomc g))) else Nothing
    Desert -> if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) && all (== Desert) (roomd g) then Just (gone{roomd=Desert:roomd g}, 1000 * (9 - length (roomd g))) else Nothing
moveFrom2 _ = Nothing

moveFrom3 :: Game -> Maybe (Game, Int)
moveFrom3 g@G{h3=Just a} =
  let gone = g{h3=Nothing}
  in case a of
    Amber -> if all (== Amber) (rooma g) then Just (gone{rooma=Amber:rooma g}, 3 - length (rooma g)) else Nothing
    Bronze -> if all (== Bronze) (roomb g) then Just (gone{roomb=Bronze:roomb g}, 10 * (3 - length (roomb g))) else Nothing
    Copper -> if isNothing (h4 g) && all (== Copper) (roomc g) then Just (gone{roomc=Copper:roomc g}, 100 * (5 - length (roomc g))) else Nothing
    Desert -> if isNothing (h4 g) && isNothing (h5 g) && all (== Desert) (roomd g) then Just (gone{roomd=Desert:roomd g}, 1000 * (7 - length (roomd g))) else Nothing
moveFrom3 _ = Nothing

moveFrom4 :: Game -> Maybe (Game, Int)
moveFrom4 g@G{h4=Just a} =
  let gone = g{h4=Nothing}
  in case a of
    Amber -> if isNothing (h3 g) && all (== Amber) (rooma g) then Just (gone{rooma=Amber:rooma g}, 5 - length (rooma g)) else Nothing
    Bronze -> if all (== Bronze) (roomb g) then Just (gone{roomb=Bronze:roomb g}, 10 * (3 - length (roomb g))) else Nothing
    Copper -> if all (== Copper) (roomc g) then Just (gone{roomc=Copper:roomc g}, 100 * (3 - length (roomc g))) else Nothing
    Desert -> if isNothing (h5 g) && all (== Desert) (roomd g) then Just (gone{roomd=Desert:roomd g}, 1000 * (5 - length (roomd g))) else Nothing
moveFrom4 _ = Nothing

moveFrom5 :: Game -> Maybe (Game, Int)
moveFrom5 g@G{h5=Just a} =
  let gone = g{h5=Nothing}
  in case a of
    Amber -> if isNothing (h3 g) && isNothing (h4 g) && all (== Amber) (rooma g) then Just (gone{rooma=Amber:rooma g}, 7 - length (rooma g)) else Nothing
    Bronze -> if isNothing (h4 g) && all (== Bronze) (roomb g) then Just (gone{roomb=Bronze:roomb g}, 10 * (5 - length (roomb g))) else Nothing
    Copper -> if all (== Copper) (roomc g) then Just (gone{roomc=Copper:roomc g}, 100 * (3 - length (roomc g))) else Nothing
    Desert -> if all (== Desert) (roomd g) then Just (gone{roomd=Desert:roomd g}, 1000 * (3 - length (roomd g))) else Nothing
moveFrom5 _ = Nothing

moveFrom6 :: Game -> Maybe (Game, Int)
moveFrom6 g@G{h6=Just a} =
  let gone = g{h6=Nothing}
  in case a of
    Amber -> if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) && all (== Amber) (rooma g) then Just (gone{rooma=Amber:rooma g}, 9 - length (rooma g)) else Nothing
    Bronze -> if isNothing (h4 g) && isNothing (h5 g) && all (== Bronze) (roomb g) then Just (gone{roomb=Bronze:roomb g}, 10 * (7 - length (roomb g))) else Nothing
    Copper -> if isNothing (h5 g) && all (== Copper) (roomc g) then Just (gone{roomc=Copper:roomc g}, 100 * (5 - length (roomc g))) else Nothing
    Desert -> if all (== Desert) (roomd g) then Just (gone{roomd=Desert:roomd g}, 1000 * (3 - length (roomd g))) else Nothing
moveFrom6 _ = Nothing

moveFrom7 :: Game -> Maybe (Game, Int)
moveFrom7 g@G{h7=Just a, h6=Nothing} =
  let gone = g{h7=Nothing}
  in case a of
    Amber -> if isNothing (h3 g) && isNothing (h4 g) && isNothing (h5 g) && all (== Amber) (rooma g) then Just (gone{rooma=Amber:rooma g}, 10 - length (rooma g)) else Nothing
    Bronze -> if isNothing (h4 g) && isNothing (h5 g) && all (== Bronze) (roomb g) then Just (gone{roomb=Bronze:roomb g}, 10 * (8 - length (roomb g))) else Nothing
    Copper -> if isNothing (h5 g) && all (== Copper) (roomc g) then Just (gone{roomc=Copper:roomc g}, 100 * (6 - length (roomc g))) else Nothing
    Desert -> if all (== Desert) (roomd g) then Just (gone{roomd=Desert:roomd g}, 1000 * (4 - length (roomd g))) else Nothing
moveFrom7 _ = Nothing

inMoves :: Game -> [(Game, Int)]
inMoves g = catMaybes [moveFrom1 g, moveFrom2 g, moveFrom3 g, moveFrom4 g, moveFrom5 g, moveFrom6 g, moveFrom7 g]

allMoves :: Game -> [(Game, Int)]
allMoves g =
  let ms = inMoves g ++ outMoves g
--  in trace (show g ++ "\n" ++ show ms ++ "\n\n") ms
  in ms

isOrderly :: Game -> Bool
isOrderly g =
  isNothing (h1 g) &&
  isNothing (h2 g) &&
  isNothing (h3 g) &&
  isNothing (h4 g) &&
  isNothing (h5 g) &&
  isNothing (h6 g) &&
  isNothing (h7 g) &&
  all (== Amber) (rooma g) &&
  all (== Bronze) (roomb g) &&
  all (== Copper) (roomc g) &&
  all (== Desert) (roomd g)

playGame :: Game -> Int
playGame init = search Set.empty (Heap.singleton (0, init))
  where
    search :: Set.Set Game -> Heap.MinPrioHeap Int Game -> Int
    search visited toBeVisited = case Heap.view toBeVisited of
      Nothing -> maxBound
      Just ((energy, game), rest) -> if isOrderly game then energy else
        if game `Set.member` visited then search visited rest else
        let visited' = Set.insert game visited
            moves =
              fmap (swap . fmap (+energy)) $
              filter (\(g, _) -> not (g `Set.member` visited)) $
              allMoves game
            toBeVisited' = foldr Heap.insert rest moves
        in search visited' toBeVisited'

input2 :: Game
input2 = G
  { h1 = Just Amber
  , h2 = Nothing
  , h3 = Nothing
  , h4 = Nothing
  , h5 = Nothing
  , h6 = Nothing
  , h7 = Nothing
  , rooma = [Bronze, Amber]
  , roomb = [Bronze]
  , roomc = [Copper, Copper]
  , roomd = [Desert, Desert] }

part23a :: IO String
part23a = do
  let x = playGame input
  return $ show x

part23b :: IO String
part23b = pure "Hello"
