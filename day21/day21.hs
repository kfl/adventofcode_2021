
module Main where

import qualified Data.List as L
import qualified Data.Function.Memoize as M
import Data.Bifunctor
import Data.Maybe (fromJust)
import Data.Tuple (swap)



test = mkGame (4,8)

input = mkGame (6,8)

data Player = Player { score :: !Int, place :: !Int } deriving (Show, Eq)


type Game = (Player, Player, Int, [Int])

mkGame :: (Int, Int) -> Game
mkGame (sp1, sp2) = (Player 0 sp1, Player 0 sp2, 0, cycle [1 .. 100])

turn (p1, p2, n, die) = (p2, p1', n+3, die')
  where
    (rolls, die') = splitAt 3 die
    newPlace = (place p1 + sum rolls - 1) `mod` 10 + 1
    p1' = Player (score p1 + newPlace) newPlace

part1 input = res
  where
    play = iterate turn input
    (p1, p2, n, _) = fromJust $ L.find (\(p1, p2, _, _) -> score p1 >= 1000 || score p2 >= 1000) play
    res = min (score p1) (score p2) * n

answer1 = part1 input


data Tree = Tree Player Player [(Int, Tree)]

sum' xs = L.foldl' (+) 0 xs

instance M.Memoizable Player where
  memoize f (Player s p) = M.memoize (f . inj) (s, p)
    where
      inj (s, p) = Player s p



tree :: (Player, Player) -> Tree
tree (p1, p2) =
  let allRolls = [ sum [r1,r2,r3] | r1 <- [1,2,3], r2 <- [1,2,3], r3 <- [1,2,3] ]
      children = [ (n, tree (p2, p1')) | group <- L.group $ L.sort allRolls,
                   let (roll, n) = (head group, fromIntegral $ length group)
                       newPlace = (place p1 + roll - 1) `mod` 10 + 1
                       p1' = Player (score p1 + newPlace) newPlace ]
  in Tree p1 p2 children

countWins (Tree p1 p2 children) =
  let (p1Wins, p2Wins) = if score p1 >= 21 then (1, 0)
                         else if score p2 >= 21 then (0, 1)
                         else bimap sum' sum' $ unzip [ (n * p1w, n*p2w)
                                                      | (n, c) <- children
                                                      , let (p2w, p1w) = countWins c]
  in (p1Wins, p2Wins)


count :: (Player, Player) -> (Int, Int)
count (p1, p2) =
  let allRolls = [ sum [r1,r2,r3] | r1 <- [1,2,3], r2 <- [1,2,3], r3 <- [1,2,3] ]
      children = [ (p1Wins, p2Wins) | group <- L.group $ L.sort allRolls,
                   let (roll, n) = (head group, length group)
                       newPlace = (place p1 + roll - 1) `mod` 10 + 1
                       newScore = score p1 + newPlace
                       p1' = Player newScore newPlace
                       (p1Wins, p2Wins) = if newScore >= 21 then (n, 0)
                                          else bimap (*n) (*n) $
                                               swap $
                                               count (p2, p1')]
  in bimap sum' sum' $ unzip children



part2 (p1, p2, _, _) = max p1w p2w
  where
--    plays = tree (p1, p2)
    (p1w, p2w) = count (p1, p2)

answer2 = part2 input

main = do
  let inp = input
  print $ part1 inp
  print $ part2 inp
