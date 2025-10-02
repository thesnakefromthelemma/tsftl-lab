{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC -Wall #-}

{- | Mutable arrays outside the GC's purview\;
    safely allocated, mutated in-place, and deallocated
    via uniqueness and linearity
-}
module Data.ForeignArray.Linear
  ( ) where