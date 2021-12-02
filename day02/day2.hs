import Data.List
import Data.Foldable (foldMap')

test = map parse ["forward 5"
                 , "down 5"
                 , "forward 8"
                 , "up 3"
                 , "down 8"
                 , "forward 2"
                 ]

input = do
  content <- readFile "input.txt"
  return $ map parse $ lines content

data Command = Forward Int
             | Down Int
             | Up Int
             deriving (Eq, Show)

parse_command str =
  case str of
    "forward" -> Forward
    "down" -> Down
    "up" -> Up

parse line = parse_command w1 $ read w2
  where [w1, w2] = words line

data Pos = Pos !Int !Int
  deriving (Show)

instance Semigroup Pos where
  (Pos x y) <> (Pos a b) = Pos (x+a) (y+b)

instance Monoid Pos where
  mempty = Pos 0 0

effect (Forward x) = Pos x 0
effect (Down x) = Pos 0 x
effect (Up x) = Pos 0 (-x)

part1 commands = hori * vert
  where Pos hori vert = foldMap effect commands

answer1 = part1 <$> input

data Aim = Aim !Int !Int !Int
  deriving (Show)

instance Semigroup Aim where
  (Aim h v a) <> (Aim x y z) = Aim (h+x) (v+y*a) (a+z)

instance Monoid Aim where -- FIXME not really a monoid
  mempty = Aim 0 0 0

effect2 (Forward x) = Aim x x 0
effect2 (Down x) = Aim 0 0 x
effect2 (Up x) = Aim 0 0 (-x)

part2_par commands = hori * vert
  where Aim hori vert _ = foldMap' effect2 commands

update (Aim h v a) (Forward x) = Aim (h+x) (v+x*a) a
update (Aim h v a) (Down x) = Aim h v (a+x)
update (Aim h v a) (Up x) = Aim h v (a-x)

part2 commands = hori * vert
  where Aim hori vert aim = foldl' update mempty commands

answer2 = part2 <$> input
