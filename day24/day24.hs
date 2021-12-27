{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Text.ParserCombinators.ReadP
import qualified Data.SBV as S


test = parse $ unlines [ "inp z"
                       , "inp x"
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
         const = ((read <$> munch1 C.isDigit) <++ (negate . read <$> (char '-' *> munch1 C.isDigit))) <* skipSpaces

         regc = (Left <$> reg) <++ (Right <$> const)


type Regs a = Map Char a
data State a = State { regs :: Regs a, next_inp :: Int }
  deriving(Eq, Show)

getInp env state = (env Map.! ("inp_"++ (show $ next_inp state)), state { next_inp = next_inp state + 1})

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
                               Eql -> \e1 e2 -> S.ite (e1 S..== e2) 1 0
                         in state{ regs = Map.insert r1 (fun e1 e2) $ regs state}

programModel prog = do
  let inputs = filter (\case Inp _ -> True; _ -> False) prog
      svars = ["inp_" ++ show i | i <- [0 .. length inputs-1] ]
  vars <- mapM S.sInt64 svars
  let zipped = zip svars vars
      env = Map.fromList zipped
  mapM_ (\ inp -> S.constrain $ inp S..> 0 S..&& inp S..< 10) vars
  let state = interp env initial prog
      z = regs state ! 'z'
  S.constrain $ z S..== 0
  let mnum = modelNo vars
  modelVar <- S.free "model number"
  S.constrain $ modelVar S..== mnum
  pure mnum

  where
    modelNo xs = foldl (\n x -> 10*n + x) 0 xs
    initial = State (Map.fromList [ (r, 0) | r <- ['w'..'z'] ]) 0


maximalModelNumber prog = S.optimize S.Lexicographic $
                          S.maximize "model number (unsigned)" =<< programModel prog

part1 prog = maximalModelNumber prog

answer1 = part1 <$> input


minimalModelNumber prog = S.optimize S.Lexicographic $
                          S.minimize "model number (unsigned)" =<< programModel prog

part2 prog = minimalModelNumber prog

answer2 = part2 <$> input

main = do
  inp <- input
  res1 <- part1 inp
  print res1
  res2 <- part2 inp
  print res2
