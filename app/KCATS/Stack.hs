{-# LANGUAGE InstanceSigs #-}
module KCATS.Stack where

import Data.List

class Stack c where
  empty :: c a
  push :: a -> c a -> c a
  pop :: c a -> Maybe (a, c a)
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
  swapI 0 s = s
  swapI _i [] = [0]
  swapI i s@(h:t) 
    | i < length s = 
      case Data.List.splitAt i s of 
        (x : xs, y : ys) -> y : xs Data.List.++ x : ys
        _ -> error "Should not happen"
    | otherwise = 0 : t Data.List.++ (take (i - length s) $ repeat 0) Data.List.++ [h]
 
  modifyTop f [] = [f 0]
  modifyTop f (x : xs) = f x : xs
  null = Data.List.null

newtype StackList a = Stack ([] a)