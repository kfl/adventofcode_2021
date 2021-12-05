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

input = map parse . lines <$> readFile "input.txt"

data Line = Line !Int !Int !Int !Int
  deriving (Show, Eq)

parse :: String -> Line
parse line = Line x1 y1 x2 y2
  where [x1, y1, x2, y2] = map read $ L.wordsBy (`elem` ", ->") line

type Pos = (Int, Int)

newtype Pic = Pic (Map Pos Int)
  deriving (Show, Eq)

instance Semigroup Pic where
  (Pic p1) <> (Pic p2) = Pic $ Map.unionWith (+) p1 p2

instance Monoid Pic where
  mempty = Pic Map.empty

fromList = Pic . Map.fromList

effect (Line x1 y1 x2 y2) = fromList [ ((x, y), 1) | (x, y) <- zip xs ys]
      where xs = mkseq x1 x2
            ys = mkseq y1 y2
            mkseq n1 n2 = case compare n1 n2 of
                            EQ -> repeat n1
                            LT -> [n1 .. n2]
                            GT -> [n1, pred n1 .. n2]


part1 lines = length $ filter (>= 2) $ Map.elems pic
  where Pic pic = foldMap effect $ filter straight lines
        straight (Line x1 y1 x2 y2) = x1 == x2 || y1 == y2

answer1 = part1 <$> input


part2 lines = length $ filter (>= 2) $ Map.elems pic
  where Pic pic = foldMap effect lines

answer2 = part2 <$> input
