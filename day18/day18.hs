
module Main where

import Control.Applicative((<|>))
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import Data.Bifunctor
import Text.ParserCombinators.ReadP

test1 = map parse [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                  , "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                  , "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                  , "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                  , "[7,[5,[[3,8],[1,4]]]]"
                  , "[[2,[2,2]],[8,[8,1]]]"
                  , "[2,9]"
                  , "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                  , "[[[5,[7,4]],7],1]"
                  , "[[[[4,2],2],6],[8,7]]"
                  ]

test2 = map parse [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
                 , "[[[5,[2,8]],4],[5,[[9,9],0]]]"
                 , "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
                 , "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
                 , "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
                 , "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
                 , "[[[[5,4],[7,7]],8],[[8,3],8]]"
                 , "[[9,3],[[9,9],[6,[4,9]]]]"
                 , "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
                 , "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
                 ]

input = map parse . lines <$> readFile "input.txt"

data Pair = Reg Int
          | Pair Pair Pair
          deriving Eq

instance Show Pair where
  show n = "parse \""++ inner n ++"\""
    where
      inner (Reg n) = show n
      inner (Pair p1 p2) = concat ["[", inner p1, ",", inner p2, "]"]

parse str = res
  where [(res, _)] = readP_to_S pair str

        pair = regular <++ actual

        actual = between (char '[') (char ']') $ do
          Pair <$> (pair <* char ',') <*> pair

        regular = Reg . read <$> munch1 C.isDigit

depth (Reg _) = 0
depth (Pair p1 p2) = 1 + max (depth p1) (depth p2)

addLeft k (Reg n) = Reg $ k+n
addLeft k (Pair p1 p2) = Pair (addLeft k p1) p2

addRight k (Reg n) = Reg $ k+n
addRight k (Pair p1 p2) = Pair p1 (addRight k p2)

explodeAt _ (Reg _) = Nothing
explodeAt level (Pair (Reg r1) (Reg r2)) | level > 3 = Just (Reg 0, r1, r2)
explodeAt level (Pair p1 p2) =
  case (explodeAt (level+1) p1, explodeAt (level+1) p2) of
    (Just (p1, r1, r2), _) -> Just (Pair p1 (addLeft r2 p2), r1, 0)
    (_, Just (p2, r1, r2)) -> Just (Pair (addRight r1 p1) p2, 0, r2)
    _ -> Nothing

splits (Reg r) | r > 9 = Just $ Pair (Reg $ r `div` 2) (Reg $ (r + 1) `div` 2)
splits (Reg _) = Nothing
splits (Pair p1 p2) =
  case (splits p1, splits p2) of
    (Just p1, _) -> Just $ Pair p1 p2
    (_, Just p2) -> Just $ Pair p1 p2
    _ -> Nothing

action n = explode <|> splits n
  where explode = fmap (\(n, _, _) -> n) $ explodeAt 0 n

reduce n = maybe n reduce $ action n

add n1 n2 = reduce $ Pair n1 n2

magnitude (Reg n) = n
magnitude (Pair p1 p2) = 3 * magnitude p1 + 2 * magnitude p2

part1 input = magnitude $ L.foldl1' add input

answer1 = part1 <$> input

part2 input = maximum res
  where
    res = [ magnitude n | x:rest <- L.tails input, y <- rest, n <- [x `add` y, y `add` x]]

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
  print $ alt_part1 inp
  print $ alt_part2 inp


-- Alternative solution -----------------------------------
--
-- Instead of representing snailfish numbers as pairs, vi can
-- represent them as a flat sequence of (Depth, Regular number)
-- pairs.

type Depth = Int
type SNum = Seq (Depth, Int)

fromPair :: Pair -> SNum
fromPair p = convert 0 p
  where convert level (Reg n) = Seq.singleton (level, n)
        convert level (Pair p1 p2) = convert (level+1) p1 >< convert (level+1) p2

addFirst _ Empty = Empty
addFirst k ((d, n) :<| rest) = (d, k+n) :<| rest

Empty `addLast` _ = Empty
(rest :|> (d,n)) `addLast` k = rest :|> (d, k+n)

alt_explode n = cursor Empty n
  where
    cursor _ Empty = Nothing
    cursor before ((5,a) :<| (5, b) :<| after) =
          Just $ before `addLast` a >< (4, 0) :<| b `addFirst` after
    cursor before (x :<| rest) = cursor (before :|> x) rest

alt_splits n = cursor Empty n
  where
    cursor _ Empty = Nothing
    cursor before ((d,r) :<| after) | r > 9 =
           Just $ before >< (d+1, r `div` 2) :<| (d+1, (r + 1) `div` 2) :<| after
    cursor before (x :<| rest) = cursor (before :|> x) rest

alt_action n = alt_explode n <|> alt_splits n

alt_reduce n = maybe n alt_reduce $ alt_action n

alt_add n1 n2 = alt_reduce $ fmap (first (+1)) (n1 >< n2)

toPair :: SNum -> Pair
toPair n = p
  where
    (p, Empty) = collect 0 n

    collect :: Depth -> SNum -> (Pair, SNum)
    collect level ((d, n) :<| rest) | level == d = (Reg n, rest)
                                    | otherwise  = (Pair p1 p2, rest'')
      where
        (p1, rest') = collect (level+1) ((d, n) :<| rest)
        (p2, rest'') = collect (level+1) rest'


alt_part1 input = magnitude .toPair $ L.foldl1' alt_add $ map fromPair input

alt_part2 input = maximum res
  where
    flat = map fromPair input
    res = [ magnitude $ toPair n | x:rest <- L.tails flat, y <- rest, n <- [x `alt_add` y, y `alt_add` x]]
