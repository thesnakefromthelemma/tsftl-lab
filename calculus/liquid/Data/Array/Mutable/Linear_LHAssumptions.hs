{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC
    -fplugin=LiquidHaskell
    -Wno-unused-imports
  #-}

module Data.Array.Mutable.Linear_LHAssumptions where

import qualified Prelude.Linear as PL

import qualified Data.Array.Mutable.Linear as W
  ( Array
  , fromList
  , size
  , unsafeSet
  , unsafeGet
  )

{-@

measure lenW :: forall a. W.Array a -> Nat

assume W.fromList :: forall a b. sa : [a] -> ({wa : W.Array a | len sa = lenW wa} -> b) -> b

assume W.size :: forall a. wa : W.Array a -> (PL.Ur ({n : Nat | lenW wa = n}), {wa' : W.Array a | lenW wa = lenW wa'})

assume W.unsafeSet :: forall a. n : Nat -> a -> {wa : W.Array a | lenW wa > n} -> {wa' : W.Array a | lenW wa = lenW wa'}

assume W.unsafeGet :: forall a. n : Nat -> {wa : W.Array a | lenW wa > n} -> (PL.Ur a, {wa' : W.Array a | lenW wa = lenW wa'})

@-}