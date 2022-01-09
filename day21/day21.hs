{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.List as L
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
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

sum' :: [Int] -> Int
sum' = L.foldl' (+) 0

allRolls :: [(Int, Int)]
allRolls = [ (roll, n)
           | group <- L.group $ L.sort [ sum [r1,r2,r3]
                                       | r1 <- [1,2,3], r2 <- [1,2,3], r3 <- [1,2,3] ]
           , let (roll, n) = (head group, length group) ]

tree :: (Player, Player) -> Tree
tree (p1, p2) = Tree p1 p2 children
  where
    children = [ (n, tree (p2, p1')) | (roll, n) <- allRolls,
                 let newPlace = (place p1 + roll - 1) `mod` 10 + 1
                     p1' = Player (score p1 + newPlace) newPlace ]


countWins (Tree p1 p2 children)
  | score p1 >= 21 = (1, 0)
  | score p2 >= 21 = (0, 1)
  | otherwise      = bimap sum' sum'
                     $ unzip [ (n * p1w, n * p2w)
                             | (n, c) <- children, let (p2w, p1w) = countWins c]


count :: (Player, Player) -> (Int, Int)
count (p1, p2) = bimap sum' sum' $ unzip children
  where
    children = [ (p1Wins, p2Wins) | (roll, n) <- allRolls,
                   let newPlace = (place p1 + roll - 1) `mod` 10 + 1
                       newScore = score p1 + newPlace
                       p1' = Player newScore newPlace
                       (p1Wins, p2Wins) = if newScore >= 21 then (n, 0)
                                          else bimap (*n) (*n) $
                                               swap $
                                               count (p2, p1')]


part2 (p1, p2, _, _) = max p1w p2w
  where
--    plays = tree (p1, p2)
    (p1w, p2w) = count (p1, p2)

answer2 = part2 input


-- Alternative strategy. We move universes forward in lockstep one
-- turn at a time. We use a multi-set to represent all universes
-- because we ultimately want to count the number of universes that
-- each player won.

data PlayerID = Player1 | Player2
              deriving (Eq, Ord, Show)
data AltPlayer = AltPlayer { who :: PlayerID, ascore :: !Int, aplace :: !Int }
               deriving (Show, Ord, Eq)
data GameState = Won PlayerID
               | Ongoing AltPlayer AltPlayer
               deriving (Show, Ord, Eq)

multicount :: AltPlayer -> AltPlayer -> (Int, Int)
multicount p1 p2 = (wins $ who p1, wins $ who p2)
  where

    turn (Ongoing p1 p2) =
      MS.fromOccurList $ [ if newScore >= 21 then (Won $ who p1, n)
                           else (Ongoing p2 p1', n)
                         | (roll, n) <- allRolls
                         , let newPlace = (aplace p1 + roll - 1) `mod` 10 + 1
                               newScore = ascore p1 + newPlace
                               p1' = p1{ ascore = newScore, aplace = newPlace } ]
    turn gs = MS.singleton gs

    universeTurn universes = MS.unionsMap turn universes

    universes = iterate universeTurn (MS.singleton $ Ongoing p1 p2)
    Just resolved = L.find (all (\case Won _ -> True; _ -> False)) universes
    wins p = MS.occur (Won p) resolved


part2' (p1, p2, _, _) = max p1w p2w
  where
    (p1w, p2w) = multicount (AltPlayer Player1 (score p1) (place p1))
                            (AltPlayer Player2 (score p2) (place p2))



main = do
  let inp = input
  putStrLn $ concat ["Part 1 answer: ", show $ part1 inp, " (expected 757770)"]
  putStrLn $ concat ["Part 2 answer: ", show $ part2' inp, " (expected 712381680443927)"]
--  print $ part2 inp
