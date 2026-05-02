{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleInstances
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wno-inaccessible-code #-}

{- | Linear low-level 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * Linear low-level 'Control.Monad.ST.runST'
    runST#
    -- * 'State#' token manipulation
  , dup#
  , Supp#
      ( supp# )
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , pattern Unlifted
  , pattern Lifted
  , RuntimeRep
      ( Int8Rep
      , Int16Rep
      , Int32Rep
      , Int64Rep
      , IntRep
      , Word8Rep
      , Word16Rep
      , Word32Rep
      , Word64Rep
      , WordRep
      , AddrRep
      , FloatRep
      , DoubleRep
      , TupleRep
      , SumRep
      , BoxedRep
      )
  , State#
  , RealWorld
  , runRW#
  )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )


-- * Linear low-level 'Control.Monad.ST.runST'

{- | Linear low-level 'Control.Monad.ST.runST' -}
{-# INLINE runST# #-}
runST# :: forall (r :: RuntimeRep) (a :: TYPE r). (forall s. State# s %1 -> a) -> a
runST# = case unsafeEqualityProof @(State# RealWorld -> a) @(State# RealWorld %1 -> a) of
    UnsafeRefl -> runRW#


-- * 'State#' token manipulation

{- | 'State#' token duplication -}
{-# INLINE dup# #-}
dup# :: forall (t :: TYPE (TupleRep '[])). t %1 -> (# t, t #)
dup# = case unsafeEqualityProof @(t -> (# t, t #)) @(t %1 -> (# t, t #)) of
    UnsafeRefl -> \ t -> (# t, t #)

{- | 'State#' token suppression\; cf. GHC-55287 as to why this must be a class -}
class Supp# (r :: RuntimeRep) where
    {- | 'State#' token suppression -}
    supp# :: forall (a :: TYPE r) (t :: TYPE (TupleRep '[])). t %1 -> a %1 -> a

#define deriveSupp(REP)                                                        \
instance Supp# (REP) where                                                     \
    {-# INLINE supp# #-}                                                       \
  ; supp# ::                                                                   \
        forall (a :: TYPE (REP)) (t :: TYPE (TupleRep '[])).                   \
        t %1 -> a %1 -> a                                                      \
  ; supp# = case unsafeEqualityProof @(t -> a %1 -> a) @(t %1 -> a %1 -> a) of \
        UnsafeRefl -> \ _ a -> a

deriveSupp(Int8Rep)
deriveSupp(Int16Rep)
deriveSupp(Int32Rep)
deriveSupp(Int64Rep)
deriveSupp(IntRep)
deriveSupp(Word8Rep)
deriveSupp(Word16Rep)
deriveSupp(Word32Rep)
deriveSupp(Word64Rep)
deriveSupp(WordRep)
deriveSupp(AddrRep)
deriveSupp(FloatRep)
deriveSupp(DoubleRep)
#define TERM '
deriveSupp(TupleRep TERM[])
deriveSupp(SumRep TERM[])
deriveSupp(BoxedRep Unlifted)
deriveSupp(BoxedRep Lifted)