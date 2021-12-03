{-# LANGUAGE LambdaCase #-}
import Data.List

test = [ "00100"
       , "11110"
       , "10110"
       , "10111"
       , "10101"
       , "01111"
       , "00111"
       , "11100"
       , "10000"
       , "11001"
       , "00010"
       , "01010"]

input = lines <$> readFile "input.txt"

data Bal = Bal [Int]
  deriving (Show, Eq)

instance Semigroup Bal where
  (Bal xs) <> (Bal ys) = Bal $ zipWith (+) xs ys

instance Monoid Bal where
  mempty = Bal $ repeat 0

effect line = Bal $ map (\case '0' -> -1; '1' -> 1) line

readout (gamma, epsilon) bal =
  if bal > 0 then (2*gamma+1, 2*epsilon) else (2*gamma, 2*epsilon+1)

part1 input = gamma * epsilon
  where Bal balances = foldMap effect input
        (gamma, epsilon) = foldl' readout (0,0) balances

answer1 = part1 <$> input

rating :: [String] -> Int -> (Int -> Bool) -> String
rating [result] _ _ = result
rating input at select = rating filtered (at+1) select
  where Bal balances = foldMap effect input
        crit = if select (head $ drop at balances) then '1' else '0'
        filtered = filter (\line -> (head $ drop at line) == crit) input

parseBin :: String -> Int
parseBin = foldl' f 0
  where f x '0' = 2*x
        f x '1' = 2*x + 1

part2 input = parseBin oxygen * parseBin co2
  where oxygen = rating input 0 (>= 0)
        co2 = rating input 0 (< 0)

answer2 = part2 <$> input
