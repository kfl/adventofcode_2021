module Day7 where

import qualified Data.List as L
import qualified Data.Vector.Unboxed as V

test = parse "16,1,2,0,4,2,7,1,2,14"

input = parse <$> readFile "input.txt"

parse :: String -> [Int]
parse str = read $ "[" ++ str ++ "]"

part1 ns = fuel
  where median = L.sort ns !! (length ns `div` 2)
        fuel = foldr (\n acc -> acc + abs(n-median)) 0 ns

answer1 = part1 <$> input

part2 ns = V.minimum costs
  where min = minimum ns
        max = maximum ns
        len = max - min + 1
        base = V.generate len (\i -> length $ filter (== i+min) ns)

        -- This is way too slow, when we know the cost at (p-1) we
        -- should be able to compute cost p efficiently
        cost p = V.generate len (\i -> let n = abs(i-p) in n*(n+1) `div` 2)
        costs = V.generate len (V.sum . V.zipWith (*) base . cost)

answer2 = part2 <$> input
