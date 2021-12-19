
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import Data.Bifunctor

test = parse <$> readFile "test.txt"
input = parse <$> readFile "input.txt"

type Point = (Int, Int, Int)

parse :: String -> [(Int, Set Point)]
parse str = scanned
  where scanners = S.splitOn "\n\n" str
        scanner s = read . L.takeWhile C.isDigit $ L.dropWhile (not . C.isDigit) s
        points ps =  map (\line -> let [x,y,z] = map read $ S.wordsBy (== ',') line in (x,y,z)) ps
        scanned = map (\ section -> let (sno:ps) = lines section in
                                    (scanner sno, Set.fromList $ points ps)) scanners

orientations = [ \ (x,y,z) -> (x,y,z)
               , \ (x,y,z) -> (x,-y,-z)
               , \ (x,y,z) -> (x,-z,y)
               , \ (x,y,z) -> (x,z,-y)
               , \ (x,y,z) -> (-x,-z,-y)
               , \ (x,y,z) -> (-x,-y,z)
               , \ (x,y,z) -> (-x,y,-z)
               , \ (x,y,z) -> (-x,z,y)
               , \ (x,y,z) -> (y,z,x)
               , \ (x,y,z) -> (y,-z,-x)
               , \ (x,y,z) -> (y,-x,z)
               , \ (x,y,z) -> (y,x,-z)
               , \ (x,y,z) -> (-y,-x,-z)
               , \ (x,y,z) -> (-y,-z,x)
               , \ (x,y,z) -> (-y,z,-x)
               , \ (x,y,z) -> (-y,x,z)
               , \ (x,y,z) -> (z,x,y)
               , \ (x,y,z) -> (z,-x,-y)
               , \ (x,y,z) -> (z,-y,x)
               , \ (x,y,z) -> (z,y,-x)
               , \ (x,y,z) -> (-z,-y,-x)
               , \ (x,y,z) -> (-z,-x,y)
               , \ (x,y,z) -> (-z,x,-y)
               , \ (x,y,z) -> (-z,y,x)
               ]


type Offset = (Int, Int, Int)

overlap :: Set Point -> (Int, Set Point) -> Maybe (Set Point)
overlap fixed (scanner_i, points) =
  case found of
    adjusted : _ -> Just adjusted -- Do we need to backtrack? I hope not
    _            -> Nothing
 where
  diff (x, y, z) (x', y', z') = (x - x', y - y', z - z')
  move (x, y, z) (x', y', z') = (x + x', y + y', z + z')

  -- All possible matches
  found =
    [ adjusted |
      a <- take (Set.size fixed - 11) $ Set.elems fixed, -- We don't need to check the last 11 elements
      trans <- orientations,
      p    <- Set.toList points,
      let offset  = a `diff` (trans p)
          adjusted = Set.map (move offset . trans) points,
      Set.size (Set.intersection fixed adjusted) >= 12 ]

match :: Set Point -> [(Int, Set Point)] -> Set Point
match fixed [] = fixed
match fixed scanners = match (Set.unions $ fixed : matched) missing
  where
    (matched, missing) = partitionEithers $
                         map (\s -> case fixed `overlap` s of
                                        Nothing -> Right s
                                        Just adjusted -> Left adjusted)
                             scanners


part1 input = Set.size $ match scanner0 rest
  where (_,scanner0) : rest = input

answer1 = part1 <$> input

part2 input = res
  where res = 42

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
