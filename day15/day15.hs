
module Main where

import Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!?))
import qualified Data.Set as Set
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.HashPSQ as Q

import Data.Bifunctor

import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))


test = map parse $ [ "1163751742"
                   , "1381373672"
                   , "2136511328"
                   , "3694931569"
                   , "7463417111"
                   , "1319128137"
                   , "1359912421"
                   , "3125421639"
                   , "1293138521"
                   , "2311944581"
                   ]

input = map parse . lines <$> readFile "input.txt"


parse = map C.digitToInt

type Grid = A.Array (Int, Int) Int

mkGrid :: [[Int]] -> Grid
mkGrid nums = A.listArray bounds $ concat nums
  where bounds = ((0,0), (length nums - 1, length (head nums) - 1))

showGrid :: Grid -> IO()
showGrid grid = do
  forM_ [0 .. maxX] $ \i -> do
    forM_ [0 .. maxY] $ \j -> do
       putStr $ show $ grid ! (i,j)
    putStrLn ""

  where (_, (maxX, maxY)) = A.bounds grid



neighbours grid (i, j) = [ idx | idx <- [(i+1, j), (i-1,j), (i, j+1), (i, j-1)]
                               , A.bounds grid `A.inRange` idx ]

x `less` may = maybe True (x <) may

bfs :: (Int, Int) -> Grid -> Map (Int, Int) Int
bfs start grid = step initial Map.empty
  where
    initial = Seq.fromList [ (n, grid ! n) | n <- neighbours grid start]
    step queue visited =
      case queue of
        Empty -> visited
        (idx, cost) :<| rest ->
          case visited !? idx of
                Nothing -> visit idx cost rest $ Map.insert idx cost visited
                Just c -> if cost < c then visit idx cost rest $ Map.insert idx cost visited
                          else visit idx c rest visited

    visit idx cost queue visited =
      step (queue >< Seq.fromList relevant) visited'
      where relevant = [ (n, cc) | n <- neighbours grid idx, let cc = cost + grid ! n, cc `less` (visited !? n) ]
            visited' = Map.fromList relevant `Map.union` visited

part1 input = res
  where
    grid = mkGrid input
    visited = bfs (0,0) grid
    (_, end) = A.bounds grid
    Just res = visited !? end

answer1 = part1 <$> input

bigger :: Grid -> Grid
bigger grid = A.array bnds [ ((i+x*(ii+1), j+y*(jj+1)), (grid ! (i,j) + x + y - 1) `mod` 9 + 1) |
                             (i, j) <- A.indices grid, x <- [0 .. 4], y <- [0 .. 4] ]
  where (origin, (ii,jj)) = A.bounds grid
        (iii, jjj) = ((ii+1) * 5 - 1, (jj+1) * 5 - 1)
        bnds = (origin, (iii, jjj))

part2 :: [[Int]] -> Int
part2 input = res
  where
    grid = mkGrid input
    big = bigger grid
    visited = bfs (0,0) big
    (_, end) = A.bounds big
    Just res = visited !? end


answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
