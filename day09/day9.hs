module Day8 where

import Data.List
import Data.Maybe (fromJust, catMaybes)
import qualified Data.List.Split as S
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector ((!?))

import Data.Monoid
import Data.Char (digitToInt)

test = V.fromList $ map parse [ "2199943210"
                              , "3987894921"
                              , "9856789892"
                              , "8767896789"
                              , "9899965678"
                              ]

input = V.fromList . map parse . lines <$> readFile "input.txt"

parse = V.fromList . map digitToInt

get ns (i, j) = ns !? i >>= (!? j)

isLow ns i j n = all (n <) $ catMaybes [ get ns idx | idx <- [(i+1, j), (i-1,j), (i, j+1), (i, j-1)]]

part1 ns = summed
  where risk i j n = if isLow ns i j n then n + 1
                     else 0
        summed = V.ifoldl' (\acc i row -> V.ifoldl' (\acc j n -> acc + risk i j n) acc row)
                           0 ns

answer1 = part1 <$> input



flood ns acc [] = acc
flood ns acc ((i,j) : xs) = flood ns acc' (neighbours ++ xs)
  where neighbours = [ idx | idx <- [(i+1, j), (i-1,j), (i, j+1), (i, (j-1))]
                           , Just n <- [get ns idx]
                           , n /= 9, Set.notMember idx acc]
        acc' = Set.union acc (Set.fromList neighbours)

part2 ns = product $ take 3 $ sortBy (flip compare) flooded
  where lows = V.ifoldl' (\acc i row -> V.ifoldl' (\acc j n -> if isLow ns i j n
                                                               then (i,j) : acc
                                                               else acc) acc row)
                           [] ns
        flooded = map (\low -> Set.size $ flood ns (Set.singleton low) [low]) lows

answer2 = part2 <$> input
