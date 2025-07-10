{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC
    -fplugin=LiquidHaskell
    -Wno-unused-imports
  #-}

module Data.Vector.Generic_LHAssumptions where

import Data.Vector.Generic.Mutable
  ( PrimMonad
  , PrimState
  )

import Data.Vector.Generic.Mutable_LHAssumptions
  ( )

import qualified Data.Vector.Generic as V
  ( Vector
  , Mutable
  , fromList
  , thaw
  )

{-@

measure lenV :: forall v a. V.Vector v a => v a -> Nat

assume V.fromList :: forall v a. V.Vector v a => sa : [a] -> {va : v a | len sa = lenV va}

assume V.thaw :: forall v m a. (PrimMonad m, V.Vector v a) => va : v a -> m ({wa : V.Mutable v (PrimState m) a | lenW wa = lenV va })

@-}