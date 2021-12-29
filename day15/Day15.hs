module Day15 where

import Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import qualified Data.HashPSQ as Q

import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))


test = map parse [ "1163751742"
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


bfs :: (Int, Int) -> Grid -> IntMap Int
bfs start grid = step initial IntMap.empty
  where
    initial = Seq.fromList [ (n, grid ! n) | n <- neighbours grid start]
    index idx = A.bounds grid `A.index` idx
    m !? idx = IntMap.lookup (index idx) m
    step queue visited =
      case queue of
        Empty -> visited
        (idx, cost) :<| rest ->
          case visited !? idx of
                Nothing -> visit idx cost rest $ IntMap.insert (index idx) cost visited
                Just c -> if cost < c then visit idx cost rest $ IntMap.insert (index idx) cost visited
                          else visit idx c rest visited

    visit idx cost queue visited =
      step (queue >< Seq.fromList relevant) visited'
      where relevant = [ (n, cc) | n <- neighbours grid idx, let cc = cost + grid ! n, cc `less` (visited !? n) ]
            visited' = IntMap.fromList [(index n, cc) | (n, cc) <- relevant] `IntMap.union` visited

part1 input = res
  where
    grid = mkGrid input
    visited = bfs (0,0) grid
    (_, end) = A.bounds grid
    index idx = A.bounds grid `A.index` idx
    m !? idx = IntMap.lookup (index idx) m
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
    grid = bigger $ mkGrid input
    visited = bfs (0,0) grid
    (_, end) = A.bounds grid
    index idx = A.bounds grid `A.index` idx
    m !? idx = IntMap.lookup (index idx) m
    Just res = visited !? end


answer2 = part2 <$> input

update n c queue = snd $ Q.alter upsert n queue
  where
    upsert Nothing = ((), Just(c, ()))
    upsert (Just(c', _)) = ((), Just(min c c', ()))

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (x,y) = abs(a-x) + abs(b-y)

aStar :: (Int, Int) -> Grid -> (Int, Int) -> Maybe Int
aStar start grid goal = loop startFrontier initPathCost
  where
    index idx = A.bounds grid `A.index` idx
    m !? idx = IntMap.lookup (index idx) m

    startFrontier = Q.singleton start 0 ()
    initPathCost = IntMap.singleton (index start) 0
    heuristic n = manhattan n goal
    stepCost n = grid ! n

    loop frontier pathCost =
      case Q.minView frontier of
        Nothing -> Nothing
        Just(s, _, _, frontier')
          | s == goal -> pathCost !? s
          | otherwise -> loop frontier'' pathCost'
          where
            Just ps = pathCost !? s
            relevant = [ (n, cc) | n <- neighbours grid s, let cc = ps + stepCost n, cc `less` (pathCost !? n) ]
            frontier'' = L.foldr (\(n, cc) front -> update n (cc + heuristic n) front) frontier' relevant
            pathCost' = IntMap.fromList [(index n, cc) | (n, cc) <- relevant] `IntMap.union` pathCost

dijkstra :: (Int, Int) -> Grid -> (Int, Int) -> Maybe Int
dijkstra start grid goal = loop startFrontier initPathCost
  where
    index idx = A.bounds grid `A.index` idx
    m !? idx = IntMap.lookup (index idx) m

    startFrontier = Q.singleton start 0 ()
    initPathCost = IntMap.singleton (index start) 0
    stepCost n = grid ! n

    loop frontier pathCost =
      case Q.minView frontier of
        Nothing -> Nothing
        Just(s, c, _, frontier')
          | s == goal -> Just c
          | otherwise -> loop frontier'' pathCost'
          where
            relevant = [ (n, cc) | n <- neighbours grid s, let cc = c + stepCost n, cc `less` (pathCost !? n) ]
            frontier'' = L.foldr (\(n, cc) front -> update n cc front) frontier' relevant
            pathCost' = IntMap.fromList [(index n, cc) | (n, cc) <- relevant] `IntMap.union` pathCost

dijkstra' :: (Int, Int) -> Grid -> (Int, Int) -> Maybe Int
dijkstra' start grid goal = loop startFrontier initPathCost
  where
    index idx = A.bounds grid `A.index` idx
    m !? idx = IntMap.lookup (index idx) m

    startFrontier = Set.singleton(0, start)
    initPathCost = IntMap.singleton (index start) 0
    stepCost n = grid ! n

    loop frontier pathCost =
      case Set.minView frontier of
        Nothing -> Nothing
        Just((c,s), frontier')
          | s == goal -> Just c
          | otherwise -> loop frontier'' pathCost'
          where
            relevant = [ (cc, n) | n <- neighbours grid s, let cc = c + stepCost n, cc `less` (pathCost !? n) ]
            frontier'' = frontier' `Set.union` Set.fromList relevant
            pathCost' = IntMap.fromList [(index n, cc) | (cc, n) <- relevant] `IntMap.union` pathCost



part2' :: [[Int]] -> Int
part2' input = res
  where
    grid = bigger $ mkGrid input
    (_, end) = A.bounds grid
    Just res = dijkstra' (0,0) grid end
