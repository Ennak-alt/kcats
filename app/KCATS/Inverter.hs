module KCATS.Inverter where

import KCATS.AST

invertInst :: INST -> INST
invertInst inst = case inst of
  ADDI int -> ADDI (-int)
  ADD -> SUB
  SUB -> ADD
  RLI int -> RRI int
  RRI int -> RLI int
  TS -> FS
  FS -> TS
  TR -> FR
  FR -> TR
  BEQ mi1 mi2 i -> BEQ mi1 mi2 (-i)
  BEZ mi1 i -> BEZ mi1 (-i)
  BGTZ mi1 i -> BGTZ mi1 (-i)
  BLTZ mi1 i -> BLTZ mi1 (-i)
  BNE mi1 mi2 i -> BNE mi1 mi2 (-i)
  BNZ mi1 i -> BNZ mi1 (-i)
  BRA i -> BRA (-i)
  HALT -> START
  START -> HALT
  _ -> inst

invertInsts :: [INST] -> [INST]
invertInsts = map invertInst . reverse