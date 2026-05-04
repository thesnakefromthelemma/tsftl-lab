{-# LANGUAGE Haskell2010
  , DataKinds
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'
-}
{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wno-inaccessible-code #-}

{- | Linear low-level 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * Linear low-level 'Control.Monad.ST.runST'
    runST#
    -- * 'State#' token manipulation
  , evST#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , RuntimeRep
      ( TupleRep )
  , pattern Many
  , pattern One
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
runST# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall s. State# s %1 -> a) -> a
runST# = case unsafeEqualityProof @(State# RealWorld %Many -> a) @(State# RealWorld %1 -> a) of
    UnsafeRefl -> runRW#


-- * 'State#' token manipulation

{- | 'State#' token manipulation -}
{-# INLINE evST# #-}
evST# ::
    forall (t :: TYPE (TupleRep '[])) (s :: RuntimeRep) (b :: TYPE s).
    (t %Many -> b) %1 -> t %1 -> b
evST# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ f t -> f t