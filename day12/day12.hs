{-# LANGUAGE LambdaCase #-}
module Day12 where

import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

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

part1 edges = length $ allPaths
  where graph = mkGraph edges
        allPaths = paths graph [] "start" "end"

answer1 = part1 <$> input


canVisit _ cave | big cave = True
canVisit _ "start" = False
canVisit seen cave = Map.notMember cave seen
                     || (all (< 2) $ Map.elems seen)

visit cave seen | big cave = seen
visit cave seen =  Map.alter (\case Nothing -> Just 1
                                    Just n -> Just $ n+1) cave seen

paths2 _ seen from to | from == to = [seen]
paths2 graph seen from to =
  let seen' = visit from seen in
  [ path | neighbour <- graph ! from, canVisit seen' neighbour
         , path <- paths2 graph seen' neighbour to]

part2 edges = length allPaths
  where graph = mkGraph edges
        allPaths = paths2 graph Map.empty "start" "end"

answer2 = part2 <$> input
