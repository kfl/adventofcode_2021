module Day8 where

import Data.List
import Data.Maybe (fromJust)
import qualified Data.List.Split as S
import qualified Data.Vector as V
import Data.Monoid

test = map parse [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                 , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
                 , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
                 , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
                 , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
                 , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
                 , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
                 , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
                 , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
                 , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
                 ]

input = map parse . lines <$> readFile "input.txt"

type Seg = String
type Entry = ([Seg], [Seg])

parse :: String -> Entry
parse str = (map sort pats, map sort outs)
  where (pats, outs) = splitAt 10 $ S.wordsBy (`elem` " |") str



part1 :: [Entry] -> Int
part1 ns = getSum known
  where known = foldMap (\(_, outs) -> Sum $ length $ knowns outs) ns
        knowns = filter (\seg -> length seg `elem` [2,3,4,7])

answer1 = part1 <$> input



-- Just some scratch notes, mapping is not really used for anything
mapping = [
            (1, "cf")
          , (7, "acf")
          , (4, "bcdf")
          , (8, "abcdefg")

          -- from 1
          , (6, "abdefg")

          -- from 1 & 4
          , (9, "abcdfg")
          , (0, "abcefg")

          -- from 4
          , (2, "acdeg")

          -- from 4 & 7
          , (3, "acdfg")
          , (5, "abdfg")
          ]

findLength n = filter ((n ==) . length)

get key = fromJust . lookup key

knowns pats = [ ('1', head $ findLength 2 pats)
              , ('4', head $ findLength 4 pats)
              , ('7', head $ findLength 3 pats)
              , ('8', head $ findLength 7 pats)]

deduce len a b pats = xp
  where candidates = filter ((len ==) . length) pats
        xp = [ ((length p1, length $ p1 \\ b), p) | p <- candidates, let p1 = p \\ a ]

-- Should/could have been written with deduce and mapping, but this value was just copied from repl
magic = [ ((5,3), '6'), ((4,2), '9'), ((4,3), '0')
        , ((3,2), '2'), ((2,1), '3'), ((3,1), '5')]

decode (pats, out) = Sum . read $ map (`get` deduced) out
  where known = knowns pats
        deduced690 = deduce 6 (get '1' known) (get '4' known) pats
        deduced235 = deduce 5 (get '7' known) (get '4' known) pats
        deduced = [ (p, n) | (c, p) <- deduced690 ++ deduced235, let n = get c magic]
                  ++ [ (p, n) | (n, p) <- known]

part2 input = getSum $ foldMap decode input

answer2 = part2 <$> input
