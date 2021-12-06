module Day6 where

import Data.List
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Mutable as MV

test = parse "3,4,3,1,2"

input = parse <$> readFile "input.txt"

parse :: String -> [Int]
parse str = read $ "[" ++ str ++ "]"

step ns = acc ++ ns'
  where (acc, ns') = mapAccumR f [] ns
        f acc 0 = (8:acc, 6)
        f acc n = (acc, n-1)

part1 ns = length $ gens !! 80
  where gens = iterate step ns

answer1 = part1 <$> input



shift = V.generate 9 (\i -> (i+1) `mod` 9)

vstep gen = V.modify (\v -> MV.modify v (\x -> x + gen ! 0) 6) $
            V.backpermute gen shift

part2 ns = sum $ gens !! 256
  where initial = V.generate 9 (\i -> length $ filter (== i) ns)
        gens = iterate vstep initial


answer2 = part2 <$> input
