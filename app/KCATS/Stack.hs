{-# LANGUAGE InstanceSigs #-}
module KCATS.Stack where

import Data.List

class Stack c where
  empty :: c a
  push :: a -> c a -> c a
  pop :: c a -> Maybe (a, c a)
  swapI 0 s = s
  swapI :: Num a => Int -> c a -> c a
  swap :: Num a => c a -> c a
  swap = swapI 1
  modifyTop :: Num a => (a -> a) -> c a -> c a
  null :: c a -> Bool

instance Stack [] where
  empty = []
  push x s = x : s
  pop :: [a] -> Maybe (a, [a])
  pop [] = Nothing
  pop (x : xs) = Just (x, xs)
  swapI :: Num a => Int -> [a] -> [a]
  swapI 0 s = s
  swapI i [] = [0]
  swapI i s@(x:xs) | i < length s = let (x : xs, y : ys) =  Data.List.splitAt i s in y : xs Data.List.++ x : ys
                   | otherwise = 0 : xs Data.List.++ (take (i - length s) $ repeat 0) Data.List.++ [x]
  modifyTop f [] = [f 0]
  modifyTop f (x : xs) = f x : xs
  null = Data.List.null

newtype StackList a = Stack ([] a)