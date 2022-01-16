{-# LANGUAGE BangPatterns, LambdaCase #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Text.ParserCombinators.ReadP
import qualified Data.SBV as S
import Data.SBV ( (.>), (.<), (.==), (.&&) )

import Data.Bifunctor (first)
import Data.Foldable (asum)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Debug.Trace


test = parse $ unlines [ "inp x"
                       , "inp z"
                       , "mul z 3"
                       , "eql z x"
                       , "add z -1"
                       ]


input = parse <$> readFile "input.txt"

type Reg = Char
type RegConst = Either Reg Int

data Opr = Add | Mul | Div | Mod | Eql deriving (Eq, Show)

data Inst = Inp Reg
          | Acc Opr Reg RegConst
          deriving (Eq, Show)

type Prog = [Inst]


parse str = res
   where [(res, _)] = readP_to_S (many inst <* eof) str
         symbol p = string p <* skipSpaces

         inst = (Inp <$> (symbol "inp" *> reg))
                <++ (Acc Add <$> (symbol "add" *> reg) <*> regc)
                <++ (Acc Mul <$> (symbol "mul" *> reg) <*> regc)
                <++ (Acc Div <$> (symbol "div" *> reg) <*> regc)
                <++ (Acc Mod <$> (symbol "mod" *> reg) <*> regc)
                <++ (Acc Eql <$> (symbol "eql" *> reg) <*> regc)


         reg = (char 'x' <++ char 'y' <++ char 'z' <++ char 'w') <* skipSpaces
         const = ((read <$> munch1 C.isDigit)
                  <++ (negate . read <$> (char '-' *> munch1 C.isDigit)))
                 <* skipSpaces

         regc = (Left <$> reg) <++ (Right <$> const)


type Regs a = Map Char a
data State a = State { regs :: Regs a, next_inp :: Int }
  deriving(Eq, Show)

getInp env state = (env Map.! ("inp_"++ show (next_inp state)),
                    state { next_inp = next_inp state + 1})

interp _ state [] = state
interp env state (inst : rest) = interp env state' rest
  where
    state' =
      case inst of
        Inp r -> let (e, s) = getInp env state
                 in s{ regs = Map.insert r e $ regs s}
        Acc opr r1 rc -> let e1 = regs state ! r1
                             e2 = case rc of
                                    Left r2 -> regs state ! r2
                                    Right n -> fromIntegral n
                             fun = case opr of
                               Add -> (+)
                               Mul -> (*)
                               Div -> S.sQuot
                               Mod -> S.sRem
                               Eql -> \e1 e2 -> S.ite (e1 .== e2) 1 0
                         in state{ regs = Map.insert r1 (fun e1 e2) $ regs state}

programModel prog = do
  let inputs = filter (\case Inp _ -> True; _ -> False) prog
      svars = ["inp_" ++ show i | i <- [0 .. length inputs-1] ]
  vars <- mapM S.sInt64 svars
  let zipped = zip svars vars
      env = Map.fromList zipped
  mapM_ (\ inp -> S.constrain $ inp .> 0 .&& inp .< 10) vars
  let state = interp env initial prog
      z = regs state ! 'z'
  S.constrain $ z .== 0
  let mnum = modelNo vars
  modelVar <- S.free "model number"
  S.constrain $ modelVar .== mnum
  pure mnum

  where
    modelNo xs = foldl (\n x -> 10*n + x) 0 xs
    initial = State (Map.fromList [ (r, 0) | r <- ['w', 'x', 'y', 'z'] ]) 0


maximalModelNumber prog = S.optimize S.Lexicographic $
                          S.maximize "model number (unsigned)" =<< programModel prog

part1 prog = maximalModelNumber prog

answer1 = part1 <$> input


minimalModelNumber prog = S.optimize S.Lexicographic $
                          S.minimize "model number (unsigned)" =<< programModel prog

part2 prog = minimalModelNumber prog

answer2 = part2 <$> input

-- ------------------------------------------------------------------
-- Alternative (slower) brute-force strategy. This corresponds roughly
-- to a breadth-first search.

newtype MinMap = MinMap (Map (Int, Int, Int, Int) Int) deriving (Eq, Show)

unMinMap (MinMap m) = m

instance Semigroup MinMap where
    (MinMap m1) <> (MinMap m2) = MinMap $ Map.unionWith min m1 m2

instance Monoid MinMap where
    mempty  = MinMap Map.empty
    mconcat ms = MinMap $ Map.unionsWith min [ m | MinMap m <- ms]


bfs_interp prog = V.minimum . V.map snd . V.filter (\(regs, _) -> lookup 'z' regs == 0) $
               loop initialStates prog
  where
    initial = (0,0,0,0)
    lookup 'z' (v, _, _, _) = v
    lookup 'w' (_, v, _, _) = v
    lookup 'x' (_, _, v, _) = v
    lookup 'y' (_, _, _, v) = v
    set 'z' !z (_, w, x, y) = (z, w, x, y)
    set 'w' !w (z, _, x, y) = (z, w, x, y)
    set 'x' !x (z, w, _, y) = (z, w, x, y)
    set 'y' !y (z, w, x, _) = (z, w, x, y)

    initialStates = V.fromList [(initial, 0)]

    orig = length prog
    lineno rest = orig - length rest
    report rest states = concat ["Executing line: ", show $ lineno rest
                                , " (keeping track of ", show $ V.length states, " states)"]

    loop states [] = states
    loop states (inst : rest) = --trace (report rest states) $
                                loop states' rest
      where
        states' =
          case inst of
            Acc opr r1 rc -> V.map (first update) states
              where
                update regs = let e1 = lookup r1 regs
                                  e2 = case rc of
                                         Left r2 -> lookup r2 regs
                                         Right n -> n
                                  fun = case opr of
                                          Add -> (+)
                                          Mul -> (*)
                                          Div -> quot
                                          Mod -> rem
                                          Eql -> \e1 e2 -> if e1 == e2 then 1 else 0
                              in set r1 (fun e1 e2) regs
            Inp r -> V.fromList . Map.toList . unMinMap $ V.foldMap' expand states
              where
                expand (regs, modelno) =
                  MinMap $ Map.fromList [ (set r v regs, modelno * 10 + v) | v <- [1..9]]




part2' prog = final
  where
    final = bfs_interp prog


-- ------------------------------------------------------------------
-- Another alternative strategy. This corresponds to a depth-first
-- search, where we use a crude value range analysis for pruning impossible
-- paths.

-- Inclusive ranges
type Range = (Int, Int)

inRange (a, b) x = a <= x && x <= b

-- Rather than making the following ad-hoc operators, we could make
-- Range an instance of the Num (+ and *) and Integral (quot and
-- rem). It might enable us merge the two interpreters, but it also
-- seems like overkill.

exactly n = (n, n)
(a,b) +: (c,d) = (a+c, b+d)
(a,b) *: (c,d) = (minimum rs, maximum rs)
  where rs = [x * y | x <- [a,b], y <- [c,d]]

(a,b) /: (c,d) = (minimum rs, maximum rs)  -- is this correct?
  where rs = [x `quot` y | x <- [a,b], y <- [c,d], y /= 0]

(a,b) %: (c,d) = (0, d-1) -- is this true for negative numbers? Also we might be able to be more precise

(a,b) ==: (c,d)
  | a == b && a == c && a == d = (1,1)
  | a <= d && c <= b = (0,1)
  | otherwise = (0, 0)

rget 'z' (v, _, _, _) = v
rget 'w' (_, v, _, _) = v
rget 'x' (_, _, v, _) = v
rget 'y' (_, _, _, v) = v

rset 'z' (_, w, x, y) !z = (z, w, x, y)
rset 'w' (z, _, x, y) !w = (z, w, x, y)
rset 'x' (z, w, _, y) !x = (z, w, x, y)
rset 'y' (z, w, x, _) !y = (z, w, x, y)

exactly_regs (z,w,x,y) = (exactly z, exactly w, exactly x, exactly y)

rzero = exactly 0
full = (1,9)

range_interp state prog = rget 'z' <$> loop state prog
  where
    loop state [] = return state
    loop state (inst : rest) =
      case inst of
        Acc opr r1 rc -> do state' <- update state
                            loop state' rest
          where
            update regs = let e1 = rget r1 regs
                              e2 = case rc of
                                     Left r2 -> rget r2 regs
                                     Right n -> exactly n
                          in rset r1 regs <$>
                             case opr of
                               Add -> return $ e1 +: e2
                               Mul -> return $ e1 *: e2
                               Div -> if e2 == rzero then Nothing else return $ e1 /: e2
                               Mod -> if e2 == rzero then Nothing else return $ e1 %: e2
                               Eql -> return $ e1 ==: e2

        Inp r -> loop (rset r state full) rest

backtrack_interp prog = loop initial prog
  where
    zero = 0
    initial = ((zero, zero, zero, zero), 0)

    full_range_set r regs = rset r (exactly_regs regs) full

    possible_inp r regs prog = case range_interp (full_range_set r regs) prog of
                                   Just rng -> rng `inRange` 0
                                   _ -> False

    loop (regs, modelno) [] = if rget 'z' regs == 0 then return modelno
                              else Nothing
    loop (regs, modelno) (inst : rest) =
      case inst of
        Acc opr r1 rc -> do regs' <- update regs
                            loop (regs', modelno) rest
          where
            update regs = let e1 = rget r1 regs
                              e2 = case rc of
                                     Left r2 -> rget r2 regs
                                     Right n -> n
                          in fmap (rset r1 regs) $
                             case opr of
                               Add -> return $ e1 + e2
                               Mul -> return $ e1 * e2
                               Div -> if e2 == 0 then Nothing else return $ e1 `quot` e2
                               Mod -> if e2 == 0 then Nothing else return $ e1 `rem` e2
                               Eql -> return $ if e1 == e2 then 1 else 0

        Inp r
          -- is it possible to assign a value in the range for r and end with 0 in the range for 'z'
          | possible_inp r regs rest ->
            asum [ loop (rset r regs v,  modelno * 10 + v) rest | v <- [1..9] ]
          | otherwise -> Nothing

part2'' prog = final
  where
    Just final = backtrack_interp prog

main = do
  inp <- input
  putStrLn "Solve part 1 with Z3"
  res1 <- part1 inp
  print res1
  putStrLn "Solve part 2 with Z3"
  res2 <- part2 inp
  print res2
  putStrLn "Solve part 2 with backtracking"
  print $ part2'' inp
  putStrLn "Solve part 2 with brute-force, this will probably take 20 mins or so"
  print $ part2' inp
