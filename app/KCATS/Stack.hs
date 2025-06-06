module KCATS.Stack where

import Data.List

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
  modifyTop f (x : xs) = f x : xs
  null = Data.List.null

newtype StackList a = Stack ([] a)