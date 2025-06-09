module KCATS.AST where

import Data.Int (Int64)

data INST
  = ADDI Int64
  | ADD
  | NEG
  | NOP
  | SUB
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
  | EXCHI Int64
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

revFallProg = [
-- Stack layout (top to bottom)
-- |    v       |   3
-- |    h       |   2
-- |    tend    |   1
-- |    t       |   0
-- Initializing stack
    START,
    FS,         
    ADDI 40,    -- velocity (v) 
    FS,          -- height (h) 
    FS,
    ADDI 4,     -- tend
    FS,         
    ADDI 4,     -- time (t)

    RBRA 3,      -- Jump to subroutine
    HALT,

-- Fall subroutine
    BRA 17,

-- Initialize loop
    SWAPBR,     -- br <=> stack[0]                          -- (which is 0)
    FR,
    NEG,
    TR,

-- Loop condition
    BGTZ Nothing 11,
    -- START,

    ADDI 1,
    -- START,

    TS,
    TS,         -- now is h
    ADDI 5,
    TS,         -- now top is v
    ADDI 10,
    FS,         -- now top is h
    -- START,
    SUB,
    -- START,
    FS,
    FS,         -- back to initial

    BNE Nothing Nothing (-11),

    BRA (-17)
  ]
