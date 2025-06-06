{-# LANGUAGE DoAndIfThenElse #-}

module KCATS.Eval where

import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits
import qualified Data.Foldable
import Data.Int (Int64)
import Data.List
import Data.Traversable (forM)
import GHC.Base (ap)
import GHC.Integer (xorInteger)
import GHC.Num (integerXor)

import KCATS.Monad
import KCATS.AST
import KCATS.Stack

runEval :: Stack c => EvalM c a () -> State c a
runEval (EvalM m) =
  (\(s, ()) -> s) $
    m (State [] empty empty empty [] (-1) 0 1)

eval [] = do
  inst <- fetchInst
  pIsEmpty <- nullP
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
      b <- nullR
      if b
        then pushP 0
        else do
          v <- popR
          pushP v
    FS -> do
      b <- nullS
      if b
        then pushP 0
        else do
          v <- popS
          pushP v
    _ | pIsEmpty -> pure ()
    EXCHI int -> do
      v <- popP
      memV <- exchMem (fromIntegral int) v
      Data.Foldable.for_ memV pushP
    ADDI int -> modP (+ int)
    SWAPBR -> do
      b <- nullR
      v <- if b then pure 0 else popR
      v' <- swapBr (fromIntegral v)
      pushR (fromIntegral v')
    XORI int -> modP (xor int)
    NEG -> modP negate
    BGTZ int -> do
      v <- peekP
      when (v > 0) $ add2Br (fromIntegral int)
    BLEZ int -> do
      v <- peekP
      when (v <= 0) $ add2Br (fromIntegral int)
    TS -> do
      v <- popP
      b <- nullS
      if v == 0 && b
        then pure ()
        else pushS v
    TR -> do
      v <- popP
      b <- nullR
      if v == 0 && b
        then pure ()
        else pushR v
    RR int -> modP (`rotateR` fromIntegral int)
    RL int -> modP (`rotateL` fromIntegral int)
    _ -> do
      v1 <- popP
      pIsEmpty <- nullP
      if pIsEmpty
        then pushP v1
        else case inst of
          SWAP -> do
            v1 <- popP
            b <- nullP
            if b
              then pushP v1
              else do
                v2 <- popP
                pushP v1
                pushP v2
          ADD -> do
            v2 <- peekP
            pushP (v1 + v2)
          SUB -> do
            v2 <- peekP
            pushP (v1 - v2)
          XOR -> do
            v2 <- peekP
            pushP (xor v1 v2)
          EXCH -> do
            v2 <- peekP
            memV <- exchMem (fromIntegral v1) v2
            pushP v1
            Data.Foldable.for_ memV pushP
          BNE int -> do
            v2 <- peekP
            pushP v1
            when (v1 /= v2) $ add2Br (fromIntegral int)
          BEQ int -> do
            v2 <- peekP
            pushP v1
            when (v1 == v2) $ add2Br (fromIntegral int)
  case inst of
    HALT -> pure ()
    _ -> eval []
eval insts = do
  initialize insts
  eval []