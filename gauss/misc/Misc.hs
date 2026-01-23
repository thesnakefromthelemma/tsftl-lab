{-# LANGUAGE Haskell2010
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Ranges done right\;
    responsible for a ton of heap allocs!
-}
module Misc
  ( -- * Ranges done right
    rangeStep
  , rangeAntiStep
  , range
  , rangeAnti
  ) where


-- + Imports

-- from base:

import GHC.List as List
  ( build )


-- * Ranges done right

{- | Range forward from first argument (inclusive)
    to second argument (exclusive) with increment
-}
{-# INLINE rangeStep #-}
rangeStep :: forall a.
    (Num a, Ord a) =>
    a -> a -> a -> [a]
rangeStep = \ i0 i1 d ->
  List.build $ \ g b ->
        let rangeStepR = \ i -> case compare i1 i of
                GT -> g i . rangeStepR $ i + d
                _  -> b
        in  rangeStepR i0

{- | Range backward from second argument (exclusive)
    to first argument (inclusive) with decrement
-}
{-# INLINE rangeAntiStep #-}
rangeAntiStep :: forall a.
    (Num a, Ord a) =>
    a -> a -> a -> [a]
rangeAntiStep = \ i0 i1 d ->
    List.build $ \ g b ->
        let rangeAntiStepR = \ i -> case compare i0 i of
                GT -> b
                _  -> g i . rangeAntiStepR $ i - d
        in  rangeAntiStepR $ i1 - d

{- | Range forward from first argument (inclusive)
    to second argument (exclusive)
-}
{-# INLINE range #-}
range :: forall a.
    (Num a, Ord a) =>
    a -> a -> [a]
range = \ i0 i1 -> rangeStep i0 i1 1

{- | Range backward from second argument (exclusive)
    to first argument (inclusive)
-}
{-# INLINE rangeAnti #-}
rangeAnti :: forall a.
    (Num a, Ord a) =>
    a -> a -> [a]
rangeAnti = \ i0 i1 -> rangeAntiStep i0 i1 1