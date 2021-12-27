
module Main where

import Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.List.Split as S
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq

import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))

import qualified Data.List as L
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either (partitionEithers)




test = parse [ "v...>>.vv>"
             , ".vv>>.vv.."
             , ">>.>v>...v"
             , ">>v>>.>.v."
             , "v>v.vv.v.."
             , ">.>>..v..."
             , ".vv..>.>v."
             , "v.v..>>v.v"
             , "....v..v.>"
             ]

input = parse . lines <$> readFile "input.txt"

type Position = (Int, Int)
type Herd = Set Position

data Seafloor = Seafloor { east :: Herd, south :: Herd, upperEast :: Int, upperSouth :: Int}
  deriving (Eq, Show)

showSeafloor seafloor = do
  forM_ [0 .. upperSouth seafloor - 1] $ \i -> do
    forM_ [0 .. upperEast seafloor - 1] $ \j -> do
      putStr $ if (i,j) `Set.member` east seafloor then ">"
               else if (i,j) `Set.member` south seafloor then "v"
                    else "."
    putStrLn ""


parse lines = Seafloor (Set.fromList eastbound) (Set.fromList southbound) upperEast upperSouth
  where
    upperSouth = length lines
    upperEast = length $ head lines
    positions = [ case c of '>' -> Left (i,j) ; 'v' -> Right (i,j)
                | (i, row) <- zip [0..] lines, (j, c) <- zip [0..] row, c `elem` ['v', '>'] ]
    (eastbound, southbound) = partitionEithers positions

step seafloor = seafloor{ east = east', south = south' }
  where
    moveEast (idx@(i, j)) = let idx' = (i, (j + 1) `mod` upperEast seafloor)
                            in if idx' `Set.notMember` east seafloor && idx' `Set.notMember` south seafloor
                               then idx' else idx
    east' = Set.map moveEast $ east seafloor
    moveSouth (idx@(i, j)) = let idx' = ((i + 1) `mod` upperSouth seafloor, j)
                             in if idx' `Set.notMember` east' && idx' `Set.notMember` south seafloor
                                then idx' else idx
    south' = Set.map moveSouth $ south seafloor

part1 input = res
  where
    steps = iterate step input
    numbered = zip [1..] $ zip steps (tail steps)
    Just (res, _) = L.find (\(_, (x,y)) -> x == y) numbered


answer1 = part1 <$> input

part2 input = res
  where
    res = "Merry Christmas"

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
