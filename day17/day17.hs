
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import Data.Ix



test :: ((Int, Int), (Int, Int))
test = ((20,30), (-10,-5))

input :: ((Int, Int), (Int, Int))
input = ((70, 125), (-159,-121)) -- x=70..125, y=-159..-121

step (xvel, yvel, x, y) = (xvel', yvel - 1, x + xvel, y + yvel)
  where xvel' = xvel + if xvel > 0 then -1 else if xvel < 0 then 1 else 0

simulate xvel yvel ((xlow,xhigh), (ylow,yhigh)) = findHighest Nothing path
  where
    start = (xvel, yvel, 0, 0)
    target = ((xlow,ylow), (xhigh,yhigh))
    path = iterate step start
    findHighest cur ((_, _, x, y) : _) | target `inRange` (x, y) = Just $ maybe y (max y) cur
    findHighest cur ((_, _, x, y) : _) | y < ylow || x > xhigh = Nothing
    findHighest cur ((_, _, x, y) : rest) = findHighest (Just $ maybe y (max y) $ cur) rest

part1 target@((xlow,xhigh), (ylow,yhigh)) = res
  where
    simulations = [ res | x <- [1 .. xhigh], y <- [ylow .. negate ylow],
                    Just res <- [simulate x y target] ]
    res = L.maximum simulations

answer1 = part1 input


part2 target@((xlow,xhigh), (ylow,yhigh)) = length simulations
  where
    simulations = [ res | x <- [1 .. xhigh], y <- [ylow .. negate ylow],
                    Just res <- [simulate x y target] ]

answer2 = part2 input

main = do
  print $ part1 input
  print $ part2 input
