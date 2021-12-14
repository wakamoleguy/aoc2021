module Day12 (part12a, part12b) where
import           Data.Char       (isLower)
import           Data.Foldable   (foldl')
import           Data.Functor    ((<&>))
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Util            (readLines)


--------------------------------------------------------------------------------
-- Day 12 - Passage Pathing
--------------------------------------------------------------------------------
data Cave = Big String | Small String deriving (Eq, Show, Ord)

input = readLines "inputs/day12.txt"

-- 1. Read input as string pairs
readEdge :: String -> (Cave, Cave)
readEdge s = (stringToCave $ head parts, stringToCave $ last parts)
  where
    parts = splitOn "-" s
    stringToCave s = if isLower (head s) then Small s else Big s

-- 2. Edges are bidirectional, so store them as (from, to) and (to, from)
type Graph = Map.Map Cave (Set.Set Cave)

addEdge :: Graph -> (Cave, Cave) -> Graph
addEdge graph (from, to) =
  let fromEdges = Map.findWithDefault Set.empty from graph
      toEdges = Map.findWithDefault Set.empty to graph
   in Map.insert from (Set.insert to fromEdges) $ Map.insert to (Set.insert from toEdges) graph

graphNeighbors :: Graph -> Cave -> Set.Set Cave
graphNeighbors graph = Set.delete (Small "start") . flip (Map.findWithDefault Set.empty) graph

-- 3. Reduce them into a Map, so I can call neighbors on them.
buildGraph :: [String] -> Graph
buildGraph lines = foldl' addEdge Map.empty $ map readEdge lines

-- 4. DFS from start to end??
paths :: Graph -> [[Cave]]
paths graph =
  let path' :: Set.Set Cave -> Cave -> [[Cave]]
      path' _ (Small "end") = [[Big "end"]]
      path' seen c =
        let neighbors = graphNeighbors graph c
            unseen = neighbors Set.\\ seen
            seen' = case c of
              Small _ -> Set.insert c seen
              Big _   -> seen
         in concatMap (map (c :) . path' seen') unseen
   in path' Set.empty (Small "start")

pathsDoubleVisit :: Graph -> [[Cave]]
pathsDoubleVisit graph =
  let path' :: Set.Set Cave -> Bool -> Cave -> [[Cave]]
      path' _ _ (Small "end") = [[Small "end"]]
      path' seen isDbl c@(Big _) =
        let neighbors = graphNeighbors graph c
         in concatMap (map (c :) . path' seen isDbl) neighbors
      path' seen False c@(Small _) =
        let neighbors = graphNeighbors graph c
            isDbl' = Set.member c seen
            seen' = Set.insert c seen
         in concatMap (map (c :) . path' seen' isDbl') neighbors
      path' seen True c@(Small _) =
        let neighbors = graphNeighbors graph c
            isDbl' = Set.member c seen
            seen' = Set.insert c seen
         in if isDbl' then [] else concatMap (map (c :) . path' seen' True) neighbors
   in path' Set.empty False (Small "start")

countPaths :: Graph -> Set.Set Cave -> Bool -> Cave -> Int
countPaths graph seen allowRepeat start =
  let choose (Small "start") = 0
      choose (Small "end") = 1
      choose c@(Small _)
        | c `Set.notMember` seen = countPaths graph (Set.insert c seen) allowRepeat c
        | allowRepeat = countPaths graph seen False c
        | otherwise = 0
      choose c@(Big _) = countPaths graph seen allowRepeat c
   in foldl' (\sum node -> sum + choose node) 0 $ graphNeighbors graph start

part12a = input <&> (\i -> countPaths (buildGraph i) Set.empty False (Small "start"))
part12b = input <&> (\i -> countPaths (buildGraph i) Set.empty True (Small "start"))
