{-# language MagicHash, TransformListComp, GADTs, TypeFamilies, PolyKinds, DataKinds #-}

module T20864 where

import Data.Kind
import GHC.Exts

loom :: [Int] -> [Int]
loom xs = [z | I# x <- xs, let p = x +# 3#, z <- [1 .. I# p], then take 3]

type family R :: Type where
  R = Type
type family V :: R where
  V = Int

-- this one requires type-family reduction in the kind to observe liftedness
cool :: [V] -> [V]
cool xs = [(x :: V) | (x :: V) <- xs, then take 3]
