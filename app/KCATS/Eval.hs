{-# LANGUAGE DoAndIfThenElse #-}

module KCATS.Eval where

import Control.Monad
import Data.Bits
import qualified Data.Foldable
import Data.Int 
import KCATS.Monad
import KCATS.AST
import KCATS.Stack

runEval :: Stack c => EvalM c a () -> State c a
runEval (EvalM m) =
  (\(s, ()) -> s) $
    m (State [] empty empty empty [] (-1) 0 1)

eval :: [INST] -> EvalM [] Int64 ()
eval [] = do
  inst <- fetchInst
  case inst of
    NOP -> pure ()
    START -> pure ()
    HALT -> pure ()
    BRA int ->
      add2Br (fromIntegral int)
    RBRA int -> do
      add2Br (fromIntegral int)
      negBr
      flipDir
    FR -> do
      v <- popR
      pushP v
    FS -> do
      v <- popS
      pushP v
    EXCHI int -> do
      v <- popP
      memV <- exchMem (fromIntegral int) v
      Data.Foldable.for_ memV pushP
    SWAPI int -> do
      swapP (fromIntegral int)
    ADDI int -> modP (+ int)
    SWAPBR -> do
      v <- popR
      v' <- swapBr (fromIntegral v)
      pushR (fromIntegral v')
    XORI int -> modP (xor int)
    NEG -> modP negate
    BEZ mi int -> do
      v <-  case mi of 
              Just i -> peekPI (fromIntegral i) 
              Nothing -> peekP
      when (v == 0) $ add2Br (fromIntegral int)
    BNZ mi int -> do
      v <-  case mi of 
              Just i -> peekPI (fromIntegral i) 
              Nothing -> peekP
      when (v /= 0) $ add2Br (fromIntegral int)
    BGTZ mi int -> do
      v <-  case mi of 
              Just i -> peekPI (fromIntegral i) 
              Nothing -> peekP
      when (v > 0) $ add2Br (fromIntegral int)
    BLTZ mi int -> do
      v <-  case mi of 
              Just i -> peekPI (fromIntegral i) 
              Nothing -> peekP
      when (v < 0) $ add2Br (fromIntegral int)
    TS -> do
      v <- popP
      pushS v
    TR -> do
      v <- popP
      pushR v
    RRI int -> modP (`rotateR` fromIntegral int)
    RLI int -> modP (`rotateL` fromIntegral int)
    SWAP -> do
      v1 <- popP
      v2 <- popP
      pushP v1
      pushP v2
    ADD -> do
      v1 <- popP
      v2 <- peekP
      pushP (v1 + v2)
    SUB -> do
      v1 <- popP
      v2 <- peekP
      pushP (v1 - v2)
    XOR -> do
      v1 <- popP
      v2 <- peekP
      pushP (xor v1 v2)
    EXCH -> do
      v1 <- popP
      v2 <- peekP
      memV <- exchMem (fromIntegral v2) v1
      Data.Foldable.for_ memV pushP
    BNE mi1 mi2 int -> do
      v1 <- case mi1 of 
              Just i  -> peekPI (fromIntegral i)
              Nothing -> peekPI 0
      v2 <- case mi2 of 
              Just i  -> peekPI (fromIntegral i)
              Nothing -> peekPI 1
      when (v1 /= v2) $ add2Br (fromIntegral int)
    BEQ mi1 mi2 int -> do
      v1 <- case mi1 of 
              Just i  -> peekPI (fromIntegral i)
              Nothing -> peekPI 0
      v2 <- case mi2 of 
              Just i  -> peekPI (fromIntegral i)
              Nothing -> peekPI 1
      when (v1 == v2) $ add2Br (fromIntegral int)
  case inst of
    HALT -> pure ()
    _ -> eval []
eval insts = do
  initialize insts
  eval []