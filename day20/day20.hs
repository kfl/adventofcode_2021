{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Vector as V
--import Data.Vector ((!))

import Data.Ix
import Data.Monoid
import qualified Data.Array.Unboxed as A
--import Data.Array.Unboxed ((!))


test = parse $ unlines [ "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##"
                       , "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###"
                       , ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#."
                       , ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#....."
                       , ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.."
                       , "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#....."
                       , "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
                       , ""
                       , "#..#."
                       , "#...."
                       , "##..#"
                       , "..#.."
                       , "..###"
                       ]

input = parse <$> readFile "input.txt"


parse str = (V.fromList . concat $ lines enhancement, pixels)
  where [enhancement, image] = S.splitOn "\n\n" str
        pixels = mkPixels '.' $ lines image

type Bit = Char
type Pixels = A.Array (Int, Int) Bit

mkPixels :: Char ->[[Char]] -> Pixels
mkPixels defaultBit bits = A.accumArray (\ _ x -> x) defaultBit bounds assoc
  where bounds = ((0,0), (length bits+1, length (head bits)+1))
        assoc = [ ((i,j), b) | (i, row) <- zip [1..] bits, (j, b) <- zip [1..] row ]

showPixels :: Pixels -> IO()
showPixels pixels = do
  forM_ [li .. ui] $ \i -> do
    forM_ [lj .. uj] $ \j -> do
       putStr $ [pixels A.! (i,j)]
    putStrLn ""

  where ((li,lj), (ui,uj)) = A.bounds pixels


parseBits :: [Bit] -> Int
parseBits = L.foldl' f 0
  where f x '.' = 2*x
        f x '#' = 2*x + 1

(!?) :: Pixels -> (Int,Int) -> Bit
pixels !? idx =
  pixels A.! if bounds `inRange` idx then idx else top
  where
    bounds@(top, _) = A.bounds pixels

stencil (i, j) = range ((i-1, j-1), (i+1, j+1))


enhance :: V.Vector Bit -> Pixels -> Pixels
enhance enhancement pixels = A.array bnds newAssocs
  where
    ((li,lj), (ui,uj)) = A.bounds pixels
    bnds = ((li-1,lj-1), (ui+1, uj+1))
    translate idx = enhancement V.! (parseBits bits)
      where bits = [ pixels !? (i,j) | (i,j) <- stencil idx ]
    newAssocs = [ (idx, translate idx) | idx <- range bnds ]


countLit pixels = getSum $ foldMap (\case '.' -> Sum 0; '#' -> Sum 1) pixels

part1 (enhancement, pixels) = countLit res
  where
    enhns = enhance enhancement
    res = enhns . enhns $ pixels

answer1 = part1 <$> input


part2 (enhancement, pixels) = countLit res
  where
    enhns = enhance enhancement
    res = iterate enhns pixels !! 50

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
