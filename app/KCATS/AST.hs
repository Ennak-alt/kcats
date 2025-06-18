module KCATS.AST where

import Data.Int (Int64)

data INST
  = ADDI Int64
  | ADD
  | SUB
  | NEG
  | NOP
  | RL 
  | RR
  | SWAP
  | XOR
  | EXCH
  | TS
  | FS
  | TR
  | FR
  | SWAPBR
  | HALT
  | START
  | SWAPI Int64
  | EXCHA Int64
  | XORI Int64
  | RLI Int64
  | RRI Int64
  | BRA Int64
  | RBRA Int64
  | BNZ  (Maybe Int64) Int64
  | BEZ  (Maybe Int64) Int64
  | BGTZ (Maybe Int64) Int64
  | BLTZ (Maybe Int64) Int64
  | BEQ (Maybe Int64) (Maybe Int64) Int64
  | BNE (Maybe Int64) (Maybe Int64) Int64
  deriving (Show, Eq)

revInst :: INST -> INST
revInst inst = case inst of
  ADDI int -> ADDI (-int)
  ADD -> SUB
  SUB -> ADD
  RLI int -> RRI int
  RRI int -> RLI int
  TS -> FS
  FS -> TS
  TR -> FR
  FR -> TR
  HALT -> START
  START -> HALT
  _ -> inst

showBranch2 :: (Show a1, Show a2) => [Char] -> Maybe a2 -> a1 -> [Char]
showBranch2 str Nothing i = str ++ " " ++ show i
showBranch2 str (Just i1) i = str ++ " $" ++ show i1 ++ " " ++ show i

showBranch3 :: (Show a1, Show a2, Show a3) => [Char] -> Maybe a2 -> Maybe a3 -> a1 -> [Char]
showBranch3 str Nothing _mi i = str ++ " " ++ show i
showBranch3 str (Just i1) Nothing i = str ++ " $" ++ show i1 ++ " " ++ show i
showBranch3 str (Just i1) (Just i2) i = 
  str ++ " $" ++ show i1 ++ " $" ++ show i2 ++ " " ++ show i

showINST :: INST -> [Char]
showINST inst = case inst of 
  BNZ  mi i -> showBranch2 "BNZ" mi i
  BEZ  mi i -> showBranch2 "BEZ" mi i
  BGTZ mi i -> showBranch2 "BGTZ" mi i
  BLTZ mi i -> showBranch2 "BLTZ" mi i
  BEQ mi1 mi2 i -> showBranch3 "BEQ" mi1 mi2 i
  BNE mi1 mi2 i -> showBranch3 "BNE" mi1 mi2 i
  _ -> filter (\c -> c /= '(' && c /= ')') $ show inst
