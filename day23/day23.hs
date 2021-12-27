
module Main where

import qualified Data.Set as Set
import qualified Data.HashPSQ as Q
import qualified Data.List as L
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as M

import Data.Hashable
--import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.Maybe (isNothing)
import Debug.Trace (trace, traceShow)

type Amphipods = Char

type Hallway = Int
hallwayPlaces = [0 .. 10] L.\\ [2,4,6,8]

type Room = Int

-- index of the top place in a room, lower place in the room is +1
topRoom 'A' = 2
topRoom 'B' = 4
topRoom 'C' = 6
topRoom 'D' = 8


-- Room places for 'A' Amphipods are dividable by 5, 'B' places by 7, 'C' places by 11, and 'D' places by 13
roomPrime room = head [ prime | prime <- [5,7,11,13], room `mod` prime == 0 ]

roomPlaces = [ prime * factor | prime <- [5,7,11,13], factor <- [1..4]]

topFor room = toHallway $ roomPrime room

roomsBelow room = [ room' | f <- [1..4], let room' = f * roomPrime room, room' > room]
roomsAbove room = [ room' | f <- [1..4], let room' = f * roomPrime room, room' < room]

depth room = room `div` roomPrime room

-- the hallway index of a room, one of the always empty places. Fx room A is under place 2
toHallway room = case roomPrime room of
                   5  -> 2
                   7  -> 4
                   11 -> 6
                   13 -> 8

-- distance from a hallway place, hall, to the room place, room.
distance hallway room = abs(hallway - toHallway room) + depth room


type Cost = Int

cost 'A' = 1
cost 'B' = 10
cost 'C' = 100
cost 'D' = 1000

data Situation = Sit { rooms :: IntMap Amphipods,
                       hallways :: IntMap Amphipods }
                 deriving (Eq, Ord, Show)

instance Hashable Situation where
  hashWithSalt s (Sit rooms hallways) = s `hashWithSalt` rooms `hashWithSalt` hallways


test = Sit (M.fromList [ (5, 'B'), (10, 'A')    -- room A
                       , (7, 'C'), (14, 'D')
                       , (11, 'B'), (22, 'C')
                       , (13, 'D'), (26, 'A') ]) -- room D
           M.empty

input = Sit (M.fromList [ (5, 'D'), (10, 'B')    -- room A
                        , (7, 'D'), (14, 'A')
                        , (11, 'C'), (22, 'B')
                        , (13, 'C'), (26, 'A') ]) -- room D
            M.empty


end = Sit (M.fromList [ (5, 'A'), (10, 'A')    -- room A
                      , (7, 'B'), (14, 'B')
                      , (11, 'C'), (22, 'C')
                      , (13, 'D'), (26, 'D') ]) -- room D
          M.empty


input2 = Sit (M.fromList [ (5, 'D'), (10, 'D'), (15, 'D'), (20, 'B')     -- room A
                         , (7, 'D'), (14, 'C'), (21, 'B'), (28, 'A')
                         , (11, 'C'), (22, 'B'), (33, 'A'), (44, 'B')
                         , (13, 'C'), (26, 'A'), (39, 'C'), (52, 'A') ]) -- room D
            M.empty


end2 = Sit (M.fromList [ (5, 'A'), (10, 'A'), (15, 'A'), (20, 'A')    -- room A
                       , (7, 'B'), (14, 'B'), (21, 'B'), (28, 'B')
                       , (11, 'C'), (22, 'C'), (33, 'C'), (44, 'C')
                       , (13, 'D'), (26, 'D'), (39, 'D'), (52, 'D') ]) -- room D
          M.empty




passage h1 h2 = [min h1 h2 .. max h1 h2]

free = isNothing

goToHallway (Sit rooms hallways) roomPlace hallway =
  if freePassage
  then Just $ (moveCost, Sit rooms' (M.insert hallway amp hallways))
  else Nothing
  where
    rooms' = M.delete roomPlace rooms
    freePassage = (all free $ map ((!?) rooms) $ roomsAbove roomPlace)
                  && (all free $ map ((!?) hallways) (passage (toHallway roomPlace) hallway))
    amp = rooms M.! roomPlace
    moveCost = cost amp * distance hallway roomPlace


goToRoom (Sit rooms hallways) roomPlace hallway =
  if freePassage
  then Just $ (moveCost, Sit (M.insert roomPlace amp rooms) hallways')
  else Nothing
  where
    hallways' = M.delete hallway hallways
    freePassage = (free $ rooms !? roomPlace)
                 && (all free $ map ((!?) rooms) $ roomsAbove roomPlace)
                 && okToEnter
                 && (all free $ map ((!?) hallways') (passage (toHallway roomPlace) hallway))
    amp = hallways M.! hallway
    okToEnter = topFor roomPlace == topRoom amp && (all (all (== amp)) $ map ((!?) rooms) $ roomsBelow roomPlace)
    moveCost = cost amp * distance hallway roomPlace

possibleMoves k sit = [ (k+c, sit') | r <- M.keys $ rooms sit, h <- hallwayPlaces
                                    , Just (c, sit') <- [goToHallway sit r h] ] ++
                      [ (k+c, sit') | h <- M.keys $ hallways sit, r <- roomPlaces
                                    , Just (c, sit') <- [goToRoom sit r h] ]


update sit c queue = snd $ Q.alter upsert sit queue
  where
    upsert Nothing = ((), Just(c, c))
    upsert (Just(c', _)) = ((), Just(mc, mc))
      where mc = min c c'


step visited k queue =
  case Q.minView queue of
    Nothing -> Nothing
    Just (sit, c, _, queue')
      | sit `Set.member` visited -> Just (sit, visited, k, queue')
      | otherwise                -> Just (sit, visited', k, queue'')
      where
        visited' = Set.insert sit visited
        moves = possibleMoves k sit
        queue'' = L.foldr (\(kc, sit') q -> update sit' kc q) queue' moves

steps start = go (step Set.empty 0 (Q.singleton start 0 0))
  where go Nothing = []
        go (Just e@(s1, v1, k1, q1)) = e : go(step v1 k1 q1)


shortest_path_faster start goal = loop Map.empty (Q.singleton start 0 0)
  where
    loop visited queue =
      case Q.minView queue of
        Nothing -> Nothing
        Just (sit, c, _, queue')
          | sit == goal              -> Just $ (c,sit)
          | Just c' <- visited Map.!? sit, c' <= c -> loop visited queue'
          | otherwise                -> loop visited' queue''
          where
            visited' = Map.insert sit c visited
            moves = possibleMoves c sit
            queue'' = L.foldr (\(kc, sit') q -> update sit' kc q) queue' moves



part1 input = c
  where
    Just(c, _) = shortest_path_faster input end

answer1 = part1 $ input

part2 input = c
  where
    Just(c, _) = shortest_path_faster input2 end2


answer2 = part2 $ input

main = do
  let inp = input
  print $ part1 inp
  print $ part2 inp
