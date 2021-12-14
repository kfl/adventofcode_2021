
module Main where

import Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Bifunctor


test = parse $ unlines [ "NNCB"
                       , ""
                       , "CH -> B"
                       , "HH -> N"
                       , "CB -> H"
                       , "NH -> C"
                       , "HB -> C"
                       , "HC -> B"
                       , "HN -> C"
                       , "NN -> C"
                       , "BH -> H"
                       , "NC -> B"
                       , "NB -> B"
                       , "BN -> B"
                       , "BB -> N"
                       , "BC -> B"
                       , "CC -> N"
                       , "CN -> C"
                       ]

input = parse <$> readFile "input.txt"

parse :: String -> (String, [(String, Char)])
parse str = (start, rules)
  where [start, srules] = S.splitOn "\n\n" str
        rules = map (\line -> let [p,x:_] = S.splitOn " -> " line in (p,x)) $ lines srules

countMap :: Ord a => [(a, Integer)] -> Map a Integer
countMap = Map.fromListWith (+)


rec_expand rules (a:b:rest) = pre ++ rec_expand rules (b:rest)
  where pre = maybe [a] (\c -> [a,c]) $ Map.lookup [a,b] rules
rec_expand rules done = done


part1 :: (String, [(String, Char)]) -> Integer
part1 (start, pairs) = maximum counts - minimum counts
  where rules = Map.fromList pairs
        after10 = iterate (rec_expand rules) start !! 10
        counts = Map.elems $ countMap $ map (\x -> (x,1)) after10

answer1 = part1 <$> input


expand rules pairs = countMap matches
  where
    matches = [ (pp,n) | (p@[a,b], n) <- Map.assocs pairs,
                         pp <- case Map.lookup p rules of
                                 Just c -> [[a,c], [c,b]]
                                 _ -> [p] ]

part2 :: (String, [(String, Char)]) -> Integer
part2 (start, lrules) = maximum counts - minimum counts
  where rules = Map.fromList lrules
        pairwise = S.divvy 2 1 start
        first = countMap $ map (\x -> (x,1)) pairwise
        after40 = iterate (expand rules) first !! 40
        counts = Map.elems $ Map.insertWith (+) (L.last start) 1 $ Map.mapKeysWith (+) head after40

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
