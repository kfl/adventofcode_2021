
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe, listToMaybe)

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

overlap :: (Offset, Set Point) -> (Int, [Set Point]) -> Maybe (Offset, Set Point)
overlap (adjustedScanner, fixed) (scanner_i, versions) = listToMaybe found
  where
    diff (x, y, z) (x', y', z') = (x - x', y - y', z - z')
    move (x, y, z) (x', y', z') = (x + x', y + y', z + z')

    -- All possible matches
    found =
      [ (offset, adjusted) |
        points <- versions,
        f <- take (Set.size fixed - 11) $ Set.elems fixed, -- We don't need to check the last 11 elements
        p <- Set.elems points,
        let offset  = f `diff` p
            adjusted = Set.mapMonotonic (move offset) points,
        Set.size (fixed `Set.intersection` adjusted) >= 12 ]

match :: [(Offset, Set Point)] -> [(Int, [Set Point])] -> [(Offset, Set Point)]
match fixed [] = fixed
match fixed scanners = match (fixed ++ matched) missing
  where
    (matched, missing) = partitionEithers $
                         map (\s -> case mapMaybe (`overlap` s) fixed of
                                      [] -> Right s
                                      adjusted : _ -> Left adjusted)
                             scanners


part1 matched = Set.size $ Set.unions $ map snd matched

--answer1 = part1 <$> input

part2 matched = maximum dists
  where
    scanners = map fst matched
    dists = [ abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
            | (x1, y1, z1) <- scanners
            , (x2, y2, z2) <- scanners
            ]

--answer2 = part2 <$> input

main = do
  inp <- input
  let (_,scanner0) : rest = inp
      versions = map (\(i, initial) -> (i, [ Set.map trans initial | trans <- orientations ])) rest
      matched = match [((0,0,0), scanner0)] versions
  print $ part1 matched
  print $ part2 matched
