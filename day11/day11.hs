module Day11 where

import Data.List
import Data.Ix
import Data.Maybe (fromJust)
import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))
import Data.Char (digitToInt)

test = map parse [ "5483143223"
                 , "2745854711"
                 , "5264556173"
                 , "6141336146"
                 , "6357385478"
                 , "4167524645"
                 , "2176841721"
                 , "6882881134"
                 , "4846848554"
                 , "5283751526"
                 ]

input = map parse . lines <$> readFile "input.txt"

parse = map digitToInt

type Grid = A.Array (Int, Int) Int

mkGrid :: [[Int]] -> Grid
mkGrid nums = A.listArray bounds $ concat nums
  where bounds = ((0,0), (length nums - 1, length (head nums) - 1))

tgrid :: Grid
tgrid = mkGrid test

neighbours grid i j = [ (x, y) | (x, y) <- range ((i-1, j-1), (i+1, j+1))
                               , A.bounds grid `inRange` (x, y)
                               , (x, y) /= (i, j) ]


increase grid idxs = (flashed, grid')
  where grid' = A.accum (\v i -> (v+i) `mod` 10) grid [ (idx, 1) | idx <- idxs ]
        flashed = [ idx | idx <- idxs, grid' ! idx == 0]


flood grid flashes [] = (grid, flashes)
flood grid flashes ((i,j) : worklist) = flood grid' flashes' (flashed ++ worklist)
  where relevant = [ idx | idx <- neighbours grid i j, grid ! idx /= 0 ]
        (flashed, grid') = increase grid relevant
        flashes' = flashes + length flashed


step :: (Grid, Int) -> (Grid, Int)
step (grid, flashes) = flood grid' (flashes + length flashed) flashed
  where (flashed, grid') = increase grid $ A.indices grid


part1 ns = snd $ steps !! 100
  where grid = mkGrid ns
        steps = iterate step (grid, 0)

answer1 = part1 <$> input


part2 ns = snd $ fromJust allFlashes
  where grid = mkGrid ns
        steps = zip (iterate step (grid, 0)) [0,1 ..]
        allFlashes = find (\((grid, _), _) -> all (0 ==) $ A.elems grid) steps

answer2 = part2 <$> input

