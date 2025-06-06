{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}

module KCATS.Monad where

import Control.Monad (liftM, unless, when)
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

import KCATS.AST
import KCATS.Stack

newtype EvalM c a b = EvalM (State c a -> (State c a, b))

data (Stack c) => State c a = State
  { instructions :: [] INST,
    primDataStack :: c a,
    secDataStack :: c a,
    retStack :: c a,
    staticMemory :: [] (Int64, a),
    pc :: Integer,
    br :: Integer,
    dir :: Integer
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

getState :: EvalM c a (State c a)
getState = EvalM $ \s -> (s, s)

peekP :: EvalM [] a a
peekP = EvalM $ \s -> (s, head $ primDataStack s)

modP :: Stack c => (a -> a) -> EvalM c a ()
modP f = EvalM $ \s -> (s { primDataStack = modifyTop f $ primDataStack s }, ())

pushP :: Stack c => a -> EvalM c a ()
pushP i = EvalM $ \s ->
  (s { primDataStack = push i $ primDataStack s }, ())

pushS :: Stack c => a -> EvalM c a ()
pushS i = EvalM $ \s ->
  (s { secDataStack = push i $ secDataStack s }, ())

pushR :: Stack c => a -> EvalM c a ()
pushR i = EvalM $ \s ->
  (s { retStack = push i $ retStack s}, ())

popP :: EvalM [] a a
popP = EvalM $ \s ->
  let (d, npds) = pop $ primDataStack s
   in (s { primDataStack = npds }, d)

popS :: EvalM [] a a
popS = EvalM $ \s ->
  let (d, nsds) = pop $ secDataStack s
   in (s { secDataStack = nsds}, d)

popR :: EvalM [] a a
popR = EvalM $ \s ->
  let (d, nrs) = pop $ retStack s
   in (s {retStack = nrs}, d)

nullS :: EvalM [] a Bool
nullS = EvalM $ \s -> (s, KCATS.Stack.null $ secDataStack s)

nullR :: EvalM [] a Bool
nullR = EvalM $ \s -> (s, KCATS.Stack.null $ retStack s)

nullP :: EvalM [] a Bool
nullP = EvalM $ \s -> (s, KCATS.Stack.null $ primDataStack s)

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

flipDir :: Stack c => EvalM c a ()
flipDir = EvalM $ \s -> (s {dir = dir s * (-1)}, ())

add2Br :: Stack c => Integer -> EvalM c a ()
add2Br i = EvalM $ \s -> (s {br = br s + i * dir s}, ())

negBr :: Stack c => EvalM c a ()
negBr = EvalM $ \s -> (s {br = - (br s)}, ())

swapBr :: Stack c => Integer -> EvalM c a Integer
swapBr i = EvalM $ \s -> (s {br = i}, br s)

initialize insts =
  let Just start = elemIndex START insts in
  EvalM $ \s -> (s {instructions = insts, pc = fromIntegral start}, ())

fetchInst :: EvalM [] a INST
fetchInst = EvalM $ \s ->
  let pc' = pc s
      dir' = dir s
      br' = br s
      insts' = instructions s
  in if pc' >= -1 && pc' <= fromIntegral (Data.List.length insts') then
      let newPC = if br' /= 0 then pc' + br' * dir' else pc' + dir' in
        case Data.List.splitAt (fromInteger newPC) insts' of
          (xs, y:ys) -> (s {pc = newPC}, if dir' == 1 then y else revInst y)
          (xs, []) -> (s {pc = newPC}, HALT)
     else error (show pc')