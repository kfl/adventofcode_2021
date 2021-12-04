module Day4 where

import Data.List
import qualified Data.List.Split as L
import Data.Function (on)
import Control.Monad (mfilter)
import Data.Maybe (fromJust, catMaybes)
import Data.Bifunctor (second)

test = parse <$> readFile "test.txt"

input = parse <$> readFile "input.txt"

data Board = Board [[Maybe Int]]
  deriving (Show, Eq)

parse :: String -> ([Int], [Board])
parse str = (map read $ L.wordsBy (==',') ns, map Board boards)
  where ns : bs = L.splitOn "\n\n" str
        boards = map (\b -> map (map (return.read) . words) $ lines b) bs



mark :: Int -> Board -> Board
mark n (Board board) = Board $ map (map $ mfilter (/= n)) board

winner (Board board) = any (all (== Nothing)) board
                       || any (all (== Nothing)) (transpose board)

part1 (ns, boards) = n * (sum $ foldMap catMaybes win)
  where marked = scanl (\(_, boards) n -> (n, map (mark n) boards)) (0, boards) ns
        (n, final) = fromJust $ find (any winner . snd) marked
        Board win = fromJust $ find winner final

answer1 = part1 <$> input



findLast (n, [board]) _ | winner board = (n, [board])
findLast (_, [board]) n = (n, [mark n board])
findLast (_, boards) n = (n, [ b | b <- map (mark n) boards, not (winner b)])

part2 (ns, boards) = n * (sum $ foldMap catMaybes safe)
  where marked = scanl findLast (0, boards) ns
        (n, [Board safe]) = fromJust $ find (any winner . snd) marked

answer2 = part2 <$> input
