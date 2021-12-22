{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Text.Scanf
import qualified Data.Set as S
import qualified Data.MultiSet as MS


test = map parse [ "on x=-20..26,y=-36..17,z=-47..7"
                 , "on x=-20..33,y=-21..23,z=-26..28"
                 , "on x=-22..28,y=-29..23,z=-38..16"
                 , "on x=-46..7,y=-6..46,z=-50..-1"
                 , "on x=-49..1,y=-3..46,z=-24..28"
                 , "on x=2..47,y=-22..22,z=-23..27"
                 , "on x=-27..23,y=-28..26,z=-21..29"
                 , "on x=-39..5,y=-6..47,z=-3..44"
                 , "on x=-30..21,y=-8..43,z=-13..34"
                 , "on x=-22..26,y=-27..20,z=-29..19"
                 , "off x=-48..-32,y=26..41,z=-47..-37"
                 , "on x=-12..35,y=6..50,z=-50..-2"
                 , "off x=-48..-32,y=-32..-16,z=-15..-5"
                 , "on x=-18..26,y=-33..15,z=-7..46"
                 , "off x=-40..-22,y=-38..-28,z=23..41"
                 , "on x=-16..35,y=-41..10,z=-47..6"
                 , "off x=-32..-23,y=11..30,z=-14..3"
                 , "on x=-49..-5,y=-3..45,z=-29..18"
                 , "off x=18..30,y=-20..-8,z=-3..13"
                 , "on x=-41..9,y=-7..43,z=-33..15"
                 , "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877"
                 , "on x=967..23432,y=45373..81175,z=27513..53682"
                 ]

input = map parse . lines <$> readFile "input.txt"

type Interval = (Int, Int)
type Cuboid = (Interval, Interval, Interval)

type Step = (Bool, Cuboid)

parse line = (posneg, ((min xl xu, max xl xu), (min yl yu, max yl yu), (min zl zu, max zl zu)))
  where
    Just (onoff :+ xl :+ xu :+ yl :+ yu :+ zl :+ zu :+ ()) =
      scanf [fmt|%s x=%d..%d,y=%d..%d,z=%d..%d|] line
    posneg = case onoff of "on" -> True; "off" -> False


doStep cubes (posneg, (xs, ys, zs)) = cubes `opr` new
  where
    interval (a, b) = [a .. b]
    new = S.fromList [ (x,y,z) | x <- interval xs, y <- interval ys, z <- interval zs ]
    opr = if posneg then S.union else \s1 s2 -> S.difference s1 (s1 `S.intersection` s2)


clamp (s@(_, ((xl, xu), (yl, yu), (zl, zu))))
  | L.all (\a -> -50 <= a && a <= 50) [xl, xu, yl, yu, zl, zu] = Just s
  | otherwise = Nothing

part1 steps = S.size cubes
  where
    cubes = L.foldl' doStep S.empty $ mapMaybe clamp steps

answer1 = part1 <$> input


isize (low, up) = up - low + 1

size (xs, ys, zs) = isize xs * isize ys * isize zs

ioverlap (a,b) (x,y) = if low > up then Nothing else Just (low, up)
  where low = max a x
        up = min b y

overlap :: Cuboid -> Cuboid -> Maybe Cuboid
overlap (as, bs, cs) (xs, ys, zs) = do
  r <- ioverlap as xs
  s <- ioverlap bs ys
  t <- ioverlap cs zs
  return (r, s, t)

doStep2 (ons, offs) (posneg, cuboid) =
  if posneg then (MS.insert cuboid $ MS.union ons offOverlap, MS.union offs onOverlap)
  else (MS.union ons offOverlap, MS.union offs onOverlap)
  where
    onOverlap = MS.mapMaybe (overlap cuboid) ons
    offOverlap = MS.mapMaybe (overlap cuboid) offs

part2 steps = res
  where
    (ons, offs) = L.foldl' doStep2 (MS.empty, MS.empty) steps
    res = sum (MS.map size ons) - sum (MS.map size offs)

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
