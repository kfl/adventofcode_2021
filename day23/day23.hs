
module Main where

import qualified Data.Set as Set
import qualified Data.OrdPSQ as Q
import qualified Data.List as L
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as M

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

roomPlaces = [topRoom 'A' .. topRoom 'D' + 1]
topFor room = room - room `mod` 2
lowerFor room = room + 1 - room `mod` 2
depth room = room `mod` 2

-- the hallway index of a room, one of the always empty places. Fx room A is under place 2
toHallway room = topFor room

-- distance from a hallway place, hall, to the room place, room.
distance hallway room = abs(hallway - toHallway room) + 1 + depth room


type Cost = Int

cost 'A' = 1
cost 'B' = 10
cost 'C' = 100
cost 'D' = 1000

data Situation = Sit { rooms :: IntMap Amphipods,
                       hallways :: IntMap Amphipods }
                 deriving (Eq, Ord, Show)


test = Sit (M.fromList [ (2, 'B'), (3, 'A')    -- room A
                       , (4, 'C'), (5, 'D')
                       , (6, 'B'), (7, 'C')
                       , (8, 'D'), (9, 'A') ]) -- room D
           M.empty

input = Sit (M.fromList [ (2, 'D'), (3, 'B')    -- room A
                        , (4, 'D'), (5, 'A')
                        , (6, 'C'), (7, 'B')
                        , (8, 'C'), (9, 'A') ]) -- room D
            M.empty


end = Sit (M.fromList [ (2, 'A'), (3, 'A')    -- room A
                      , (4, 'B'), (5, 'B')
                      , (6, 'C'), (7, 'C')
                      , (8, 'D'), (9, 'D') ]) -- room D
          M.empty

step1 = Sit (M.fromList [ (2,'B'), (3, 'A')    -- room A
                       , (4, 'C'), (5, 'D')
                       , (7, 'C')
                       , (8, 'D'), (9, 'A') ]) -- room D
                   (M.fromList [(3,'B')])

step15 = Sit (M.fromList [ (2,'B'), (3, 'A')    -- room A
                        , (5, 'D')
                        , (7, 'C')
                       , (8, 'D'), (9, 'A') ]) -- room D
                   (M.fromList [(3,'B'), (5, 'C')])


step2 = Sit (M.fromList [ (2,'B'), (3, 'A')    -- room A
                        , (5, 'D')
                        , (6, 'C'), (7, 'C')
                       , (8, 'D'), (9, 'A') ]) -- room D
                   (M.fromList [(3,'B')])

step3 = Sit (M.fromList [ (2,'B'), (3, 'A')    -- room A
                        , (5, 'B')
                        , (6, 'C'), (7, 'C')
                       , (8, 'D'), (9, 'A') ]) -- room D
                   (M.fromList [(5,'D')])

step4 = Sit (M.fromList [ (3, 'A')    -- room A
                        , (4, 'B'), (5, 'B')
                        , (6, 'C'), (7, 'C')
                       , (8, 'D'), (9, 'A') ]) -- room D
                   (M.fromList [(5,'D')])


step5 = Sit {rooms = M.fromList [(3,'A'), (4, 'B'), (5, 'B'), (6, 'C'), (7, 'C')], hallways = M.fromList [(5,'D'),(7,'D'),(9, 'A')]}


passage h1 h2 = [min h1 h2 .. max h1 h2]

free = isNothing

goToHallway (Sit rooms hallways) roomPlace hallway =
  if freePassage
  then Just $ (moveCost, Sit rooms' (M.insert hallway amp hallways))
  else Nothing
  where
    rooms' = M.delete roomPlace rooms
    freePassage = (free $ rooms' !? topFor roomPlace)
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
                 && (free $ rooms !? topFor roomPlace)
                 && okToEnter
                 && (all free $ map ((!?) hallways') (passage (toHallway roomPlace) hallway))
    amp = hallways M.! hallway
    okToEnter = topFor roomPlace == topRoom amp && all (== amp) (rooms !? lowerFor roomPlace)
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

part2 input = res
  where
    res = 42

answer2 = part2 $ input

main = do
  let inp = input
  print $ part1 inp
  print $ part2 inp
