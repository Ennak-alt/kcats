{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module KCATS.AST where

import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits
import qualified Data.Foldable
import Data.Int (Int64)
import Data.List
import Data.Traversable (forM)
import Data.Vector.Mutable
import qualified Data.Vector.Primitive as Data
import GHC.Base (ap)
import GHC.Integer (xorInteger)
import GHC.Num (integerXor)

-- import Data.Vector

class Stack c where
  empty :: c a
  push :: a -> c a -> c a
  pop :: c a -> (a, c a)
  swapI 0 s = s
  swapI :: Int -> c a -> c a
  swap :: c a -> c a
  swap = swapI 1
  modifyTop :: (a -> a) -> c a -> c a
  null :: c a -> Bool

instance Stack [] where
  empty = []
  push x s = x : s
  pop (x : xs) = (x, xs)
  swapI 0 s = s
  swapI i s = let (x : xs, y : ys) = Data.List.splitAt i s in y : xs Data.List.++ x : ys
  modifyTop :: (a -> a) -> [a] -> [a]
  modifyTop f (x : xs) = f x : xs
  null = Data.List.null

newtype StackList a = Stack ([] a)

newtype EvalM c a b = EvalM (State c a -> (State c a, b))

data (Stack c) => State c a = State
  { instructions :: [] INST,
    primDataStack :: c a,
    secDataStack :: c a,
    retStack :: c a,
    staticMemory :: [] (Int64, a),
    pc :: Integer,
    dir :: Integer,
    br :: Integer
  }
  deriving Show

instance Functor (EvalM c a) where
  fmap = liftM

instance Applicative (EvalM c a) where
  pure x = EvalM (,x)
  (<*>) = ap

instance Monad (EvalM c a) where
  return = pure
  EvalM x >>= f = EvalM $ \state ->
    let (s, x') = x state
        EvalM y = f x'
     in y s

getState = EvalM $ \s -> (s, s)

peekP :: EvalM [] a a
peekP = EvalM $ \s -> (s, head $ primDataStack s)

modP :: Stack c => (a -> a) -> EvalM c a ()
modP f = EvalM $ \s -> (s { primDataStack = modifyTop f $ primDataStack s }, ())

pushP i = EvalM $ \s ->
  (s { primDataStack = push i $ primDataStack s }, ())

pushS i = EvalM $ \s ->
  (s { secDataStack = push i $ secDataStack s }, ())

pushR i = EvalM $ \s ->
  (s { retStack = push i $ retStack s}, ())

popP = EvalM $ \s ->
  let (d, npds) = pop $ primDataStack s
   in (s { primDataStack = npds }, d)

popS :: EvalM [] b b
popS = EvalM $ \s ->
  let (d, nsds) = pop $ secDataStack s
   in (s { secDataStack = nsds}, d)

popR :: EvalM [] b b
popR = EvalM $ \s ->
  let (d, nrs) = pop $ retStack s
   in (s {retStack = nrs}, d)

nullS = EvalM $ \s -> (s, KCATS.AST.null $ secDataStack s)

nullP :: EvalM [] a Bool
nullP = EvalM $ \s -> (s, KCATS.AST.null $ primDataStack s)

exchMem :: (Stack c, Eq b, Num b) => Int64 -> b -> EvalM c b (Maybe b)
exchMem i v = EvalM $ \s ->
  case lookup i $ staticMemory s of
    Just elem ->
      if v == 0
        then
          (s {staticMemory = Data.List.filter ((/= i) . fst) $ staticMemory s}, Just elem)
        else
          (s {staticMemory = Data.List.map (\(key, y) -> if key == i then (key, v) else (key, y)) $ staticMemory s}, Just elem)
    Nothing -> if v == 0 then (s, Just 0) else (s { staticMemory = (i, v) : staticMemory s}, Just 0)

initialize insts = EvalM $ \s -> (s {instructions = insts}, ())

fetchInst = EvalM $ \s ->
  let pc' = pc s
      dir' = dir s
      br' = br s
      insts' = instructions s
  in if pc' >= 0 && pc' <= fromIntegral (Data.List.length insts') then
      if pc' == fromIntegral (Data.List.length insts') then (s, HALT)
      else if br' /= 0 then
        let newPC = (pc' + br' * dir')
            (xs, y:ys) = Data.List.splitAt (fromIntegral newPC) insts'
        in (s, y)
      else if br' == 0 then
            let newPC = (pc' + dir')
                (xs, y:ys) = Data.List.splitAt (fromIntegral newPC) insts'
            in (s, y)
      else error "Jumped outside of program!"
     else error "Jumped outside of program!"

data INST
  = ADDI Int64
  | ADD
  | SUB
  | SWAP
  | XOR
  | XORI Int64
  | RL Int64
  | RR Int64
  | EXCHB Int64
  | EXCH Int64
  | MOVTS
  | MOVFS
  | BRA Int64
  | HALT
  deriving (Show)

runEval (EvalM m) =
  (\(s, ()) -> s) $
    m (State [] KCATS.AST.empty KCATS.AST.empty KCATS.AST.empty [] 0 0 1)

eval [] = do
  inst <- fetchInst
  pIsEmpty <- nullP
  case inst of
    HALT -> pure ()
    MOVFS -> do
      b <- nullS
      if b
        then pushP 0
        else do
          v <- popS
          pushP v
    _ | pIsEmpty -> pure ()
    EXCH int -> do
      v <- popP
      memV <- exchMem (fromIntegral int) v
      Data.Foldable.for_ memV pushP
    ADDI int -> modP (+ int)
    XORI int -> modP (xor int)
    MOVTS -> do
      v <- popP
      b <- nullS
      if v == 0 && b
        then pure ()
        else pushS v
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
  case inst of 
    HALT -> pure ()
    _ -> eval []
eval insts = do
  initialize insts
  eval []
