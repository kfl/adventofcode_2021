
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import Text.Printf (printf)
import Text.ParserCombinators.ReadP
import Control.Monad (replicateM)


parseBin :: String -> Int
parseBin = L.foldl' f 0
  where f x '0' = 2*x
        f x '1' = 2*x + 1

test = "D2FE28"

input = L.dropWhileEnd C.isSpace <$> readFile "input.txt"

toBits :: String -> String
toBits str = concatMap (printf "%04b") $ map C.digitToInt str

type Version = Int
type Operation = Int

data Packet = Lit Version Int
            | Opr Version Operation [Packet]
            deriving (Eq, Show)

getn n = replicateM n get

getInt3 = parseBin <$> getn 3

version = getInt3

readChunks = do
  c <- get
  case c of
    '1' -> (++) <$> getn 4 <*> readChunks
    '0' -> getn 4

packet = do
  ver <- version
  typ <- getInt3
  case typ of
    4   -> Lit ver . parseBin <$> readChunks
    opr -> Opr ver opr <$> subPackets

len = do
  c <- get
  case c of
    '0' -> Left . parseBin <$> getn 15
    '1' -> Right . parseBin <$> getn 11

subPackets = do
  l <- len
  case l of
    Right n -> count n packet
    Left n -> parse (gobble packet) <$> getn n

gobble p = (:) <$> p <*> gobble p <++ return []

parse p str = res
  where [(res, _)] = readP_to_S (p <* munch (== '0')) str


versionSum (Lit ver _) = ver
versionSum (Opr ver _ subs) = ver + (sum $ map versionSum subs)

part1 input = versionSum p
  where
    bits = toBits input
    p = parse packet bits

answer1 = part1 <$> input


compute (Lit _ n) = n
compute (Opr _ opr subs) = fun $ map compute subs
  where
    fun = case opr of
            0 -> sum
            1 -> product
            2 -> minimum
            3 -> maximum
            5 -> \ [x, y] -> if x > y then 1 else 0
            6 -> \ [x, y] -> if x < y then 1 else 0
            7 -> \ [x, y] -> if x == y then 1 else 0

part2 input = compute p
  where
    bits = toBits input
    p = parse packet bits

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
