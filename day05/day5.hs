module Day5 where

import Data.List
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

test = map parse [ "0,9 -> 5,9"
                 , "8,0 -> 0,8"
                 , "9,4 -> 3,4"
                 , "2,2 -> 2,1"
                 , "7,0 -> 7,4"
                 , "6,4 -> 2,0"
                 , "0,9 -> 2,9"
                 , "3,4 -> 1,4"
                 , "0,0 -> 8,8"
                 , "5,5 -> 8,2"
                 ]

input = map parse <$> lines <$> readFile "input.txt"

data Line = Line !Int !Int !Int !Int
  deriving (Show, Eq)

parse :: String -> Line
parse line = Line x1 y1 x2 y2
  where [x1, y1, x2, y2] = map read $ L.wordsBy (flip elem ", ->") line

type Pos = (Int, Int)

data Pic = Pic (Map Pos Int)
  deriving (Show, Eq)

instance Semigroup Pic where
  (Pic p1) <> (Pic p2) = Pic $ Map.unionWith (+) p1 p2

instance Monoid Pic where
  mempty = Pic Map.empty

effect (Line x1 y1 x2 y2) | x1 == x2 =
      Pic $ Map.fromList [ ((x1, y), 1) | y <- [min y1 y2 .. max y1 y2]]
effect (Line x1 y1 x2 y2) | y1 == y2 =
      Pic $ Map.fromList [ ((x, y1), 1) | x <- [min x1 x2 .. max x1 x2]]
effect _ = mempty

part1 lines = length $ filter (>= 2) $ Map.elems pic
  where Pic pic = foldMap effect lines

answer1 = part1 <$> input



effect2 (Line x1 y1 x2 y2) | x1 == x2 =
      Pic $ Map.fromList [ ((x1, y), 1) | y <- [min y1 y2 .. max y1 y2]]
effect2 (Line x1 y1 x2 y2) | y1 == y2 =
      Pic $ Map.fromList [ ((x, y1), 1) | x <- [min x1 x2 .. max x1 x2]]
effect2 (Line x1 y1 x2 y2) =
      Pic $ Map.fromList [ ((x, y), 1)
                         | (x, y) <- zip [x1, xstep x1 .. x2]
                                         [y1, ystep y1 .. y2]]
      where xstep = if x1 > x2 then pred else succ
            ystep = if y1 > y2 then pred else succ

part2 lines = length $ filter (>= 2) $ Map.elems pic
  where Pic pic = foldMap effect2 lines

answer2 = part2 <$> input
