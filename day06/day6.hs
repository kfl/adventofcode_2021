module Day6 where

import qualified Data.List as L
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

test = parse "3,4,3,1,2"

input = parse <$> readFile "input.txt"

parse :: String -> [Int]
parse str = read $ "[" ++ str ++ "]"

step ns = acc ++ ns'
  where (acc, ns') = L.mapAccumR f [] ns
        f acc 0 = (8:acc, 6)
        f acc n = (acc, n-1)

part1 ns = length $ gens !! 80
  where gens = iterate step ns

answer1 = part1 <$> input



shift = V.generate 9 (\i -> (i+1) `mod` 9)
(vec, i) += c = V.accum (+) vec [(i,c)]

vstep gen = (shifted, 6) += gen0
  where gen0 = gen ! 0
        shifted = V.backpermute gen shift

part2 ns = V.sum $ gens !! 256
  where initial = V.generate 9 (\i -> length $ filter (== i) ns)
        gens = iterate vstep initial


answer2 = part2 <$> input
