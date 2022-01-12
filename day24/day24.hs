{-# LANGUAGE BangPatterns, LambdaCase #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Text.ParserCombinators.ReadP
import qualified Data.SBV as S
import Data.SBV ( (.>), (.<), (.==), (.&&) )

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

getInp env state = (env Map.! ("inp_"++ (show $ next_inp state)),
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

-- Alternative (slower) brute-force strategy

newtype MinMap = MinMap (Map (Int, Int, Int, Int) Int) deriving (Eq, Show)

unMinMap f (MinMap m) = f m

instance Semigroup MinMap where
    (MinMap m1) <> (MinMap m2) = MinMap $ Map.unionWith min m1 m2

instance Monoid MinMap where
    mempty  = MinMap Map.empty
    mconcat ms = MinMap $ Map.unionsWith min [ m | MinMap m <- ms]


ainterp prog = V.minimum . V.map snd . V.filter (\(regs, _) -> lookup 'z' regs == 0) $
               loop initialStates prog
  where
    initial = (0,0,0,0)
    lookup 'z' !(v, _, _, _) = v
    lookup 'w' !(_, v, _, _) = v
    lookup 'x' !(_, _, v, _) = v
    lookup 'y' !(_, _, _, v) = v
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
            Acc opr r1 rc -> V.map (\(regs, modelno) -> (update regs, modelno)) states
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
            Inp r -> V.fromList . unMinMap Map.toList $ V.foldMap' expand states
              where
                expand (regs, modelno) =
                  MinMap $ Map.fromList [ (set r v regs, modelno * 10 + v) | v <- [1..9]]

part2' prog = final
  where
    final = ainterp prog


main = do
  inp <- input
  res1 <- part1 inp
  print res1
  res2 <- part2 inp
  print res2
  putStrLn "Get ready for some brute-forcing, this will probably take 20 mins or so"
  print $ part2' inp
