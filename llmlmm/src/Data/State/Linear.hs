{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleInstances
  , GADTSyntax
  , GHCForeignImportPrim
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
#-}


{- | Low-level linear 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear allocation tokens
    Alloc#
    -- * Running 'Alloc#' and 'State#'
  , runLA#
  , RunST#
      ( runST# )
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( RuntimeRep
  , TYPE
  , State#
  , runRW#
  , Multiplicity
      ( One
      , Many
      )
  )

import Data.Coerce
  ( coerce )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ (internal)

import Prelude.Linear
  ( Urlike ( .. ) )

import Data.State.Linear.TH
  ( Alloc#
      ( Alloc# )
  , declareUrlikeAlloc#
  )


-- * TemplateHaskell generation of @forall t. 'Urlike' ('Alloc#' t)@ instance

{- | Declares @forall t. 'Urlike' ('Alloc#' t)@ -}
$( declareUrlikeAlloc# )


-- * Running 'Alloc#' and 'State#'

{- | Running 'Alloc#' -}
{-# INLINE runLA# #-}
runLA# ::
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall t. Alloc# t %One-> a) %One-> a
runLA# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce runRW#

{- | Running 'State#' -}
class RunST# (p :: Multiplicity) where
    {- | Multiplicity polymorphic 'Control.Monad.ST.runST' -}
    runST# :: forall (r :: RuntimeRep) (a :: TYPE r).
        (forall t. State# t %p-> a) %One-> a
instance RunST# One where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> coerce runRW#
instance RunST# Many where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> coerce runRW#