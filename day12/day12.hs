{-# LANGUAGE LambdaCase #-}
module Day12 where

import Data.Char (isUpper)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set


test = map parse [ "start-A"
                 , "start-b"
                 , "A-c"
                 , "A-b"
                 , "b-d"
                 , "A-end"
                 , "b-end"
                 ]

test2 = map parse [ "dc-end"
                  , "HN-start"
                  , "start-kj"
                  , "dc-start"
                  , "dc-HN"
                  , "LN-dc"
                  , "HN-end"
                  , "kj-sa"
                  , "kj-HN"
                  , "kj-dc"
                  ]

input = map parse . lines <$> readFile "input.txt"

parse line = (x,y)
  where [x, y] = words $ map (\case '-' -> ' '; c -> c) line

type Cave = String
type Graph = Map Cave [Cave]
type Path = [Cave]

mkGraph :: [(Cave,Cave)] -> Graph
mkGraph edges =
  Map.fromListWith (++) [ (k, adj) | (a,b) <- edges
                                   , (k, adj) <- [(a, [b]), (b, [a])] ]

big :: Cave -> Bool
big = isUpper . head

visited seen cave | big cave = False
visited seen cave = cave `elem` seen


paths :: Graph -> Path -> Cave -> Cave -> [Path]
paths _ seen from to | from == to = [seen]
paths graph seen from to =
  [ path | neighbour <- graph ! from, not $ visited seen neighbour
         , path <- paths graph (from : seen) neighbour to]

part1 edges = length allPaths
  where graph = mkGraph edges
        allPaths = paths graph [] "start" "end"

answer1 = part1 <$> input


data Capacity a = Free a
                | Full a
                deriving (Show, Eq)

canVisit _ cave | big cave = True
canVisit _ "start" = False
canVisit (Free seen) cave = True
canVisit (Full seen) cave = cave `Set.notMember` seen

visit cave seen | big cave = seen
visit cave (Free seen) = if cave `Set.member` seen then Full $ seen
                         else Free $ Set.insert cave seen
visit cave (Full seen) = Full $ Set.insert cave seen

paths2 _ seen from to | from == to = [1]
paths2 graph seen from to =
  [ path | neighbour <- graph ! from, canVisit seen' neighbour
         , path <- paths2 graph seen' neighbour to]
  where
    seen' = visit from seen


part2 edges = length allPaths
  where graph = mkGraph edges
        allPaths = paths2 graph (Free Set.empty) "start" "end"

answer2 = part2 <$> input
