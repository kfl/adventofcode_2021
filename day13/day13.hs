
module Main where

import Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Set as Set
import Data.Bifunctor


test = parse $ unlines [ "6,10"
                       , "0,14"
                       , "9,10"
                       , "0,3"
                       , "10,4"
                       , "4,11"
                       , "6,0"
                       , "6,12"
                       , "4,1"
                       , "0,13"
                       , "10,12"
                       , "3,4"
                       , "3,0"
                       , "8,4"
                       , "1,10"
                       , "2,14"
                       , "8,10"
                       , "9,0"
                       , ""
                       , "fold along y=7"
                       , "fold along x=5"
                       ]

input = parse <$> readFile "input.txt"

data Folding = Vert Int | Hori Int
  deriving (Eq, Show)
type Dot = (Int,Int)

parse :: String -> ([Dot], [Folding])
parse str = (points, foldings)
  where [sptr, sfolds] = S.splitOn "\n\n" str
        points = map (bimap read (read . tail) . L.break (== ',')) $ lines sptr
        parseFolding s | "fold along y=" `L.isPrefixOf` s = Hori . read $ L.dropWhile (not . C.isDigit) s
        parseFolding s | "fold along x=" `L.isPrefixOf` s = Vert . read $ L.dropWhile (not . C.isDigit) s
        foldings = map parseFolding $ lines sfolds

foldDots dots folding = Set.map trans dots
  where f n x = if x > n then 2*n - x else x
        trans = case folding of
                  Hori n -> second $ f n
                  Vert n -> first $ f n

part1 (ldots, f : _) = Set.size folded
  where dots = Set.fromList ldots
        folded = foldDots dots f

answer1 = part1 <$> input

showDots dots = do
  putStrLn ""
  putStrLn ""
  forM_ [0 .. maxY] $ \j -> do
    forM_ [0 .. maxX] $ \i -> do
      if (i,j) `Set.member` dots then putStr "#"
      else putStr " "
    putStrLn ""

  where maxX = maximum $ Set.map fst dots
        maxY = maximum $ Set.map snd dots

part2 (ldots, foldings) = showDots folded
  where dots = Set.fromList ldots
        folded = L.foldl foldDots dots foldings

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  part2 inp
