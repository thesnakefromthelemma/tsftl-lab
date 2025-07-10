{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC
    -fplugin=LiquidHaskell
    -Wno-unused-imports
  #-}

module Data.Vector.Generic.Mutable_LHAssumptions where

import Data.Vector.Generic.Mutable
  ( PrimMonad
  , PrimState
  )

import qualified Data.Vector.Generic.Mutable as W
  ( MVector
  , unsafeInit
  , length
  , unsafeWrite
  , unsafeRead
  )

{-@

measure lenW :: forall w s a. W.MVector w a => w s a -> Nat

assume W.length :: forall w s a. W.MVector w a => wa : w s a -> {n : Nat | lenW wa = n}

assume W.unsafeInit :: forall w s a. W.MVector w a => {wa : w s a | 1 <= lenW wa} -> {wa' : w s a | lenW wa - 1 = lenW wa'}

assume W.unsafeWrite :: forall w m a. (W.MVector w a, PrimMonad m) => wa : w (PrimState m) a -> {n : Nat | lenW wa > n} -> a -> m ()

assume W.unsafeRead :: forall w m a. (W.MVector w a, PrimMonad m) => wa : w (PrimState m) a -> {n : Nat | lenW wa > n} -> m a

@-}