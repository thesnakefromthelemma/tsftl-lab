{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleInstances
  , GADTSyntax
  , GHCForeignImportPrim
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , TemplateHaskell
  , TypeApplications
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
    @-Worphans@ is disabled so that we can
    generate an 'Urlike' instance for (@forall t.@) @'Alloc#' t@
    (defined in "Prelude.Linear")
    in this module ("Data.State.Linear")
    for a type defined in ""Data.State.Linear.TH"\;
    this is safe because 'Alloc#' is
    exported outside this package solely by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
    -Wno-orphans
#-}

{- | Low-level linear 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens
    Alloc#
    -- * Running 'Alloc#' and 'State#'
  , runLA#
  , RunST#
      ( runST# )
  , runLAST#
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
    UnsafeRefl -> \ x -> runRW# (\ s -> x (Alloc# s))

{- | Running 'State#' -}
class RunST# (p :: Multiplicity) where
    {- | Multiplicity polymorphic 'Control.Monad.ST.runST' -}
    runST# :: forall (r :: RuntimeRep) (a :: TYPE r).
        (forall t. State# t %p-> a) %One-> a
instance RunST# One where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> \ x -> runRW# (\ s -> x s)
instance RunST# Many where
    {-# INLINE runST# #-}
    runST# = case unsafeEqualityProof @Many @One of
        UnsafeRefl -> \ x -> runRW# (\ s -> x s)

{- | Running 'Alloc#' and 'State#' -}
{-# INLINE runLAST# #-}
runLAST# ::
    forall (p :: Multiplicity). RunST# p =>
    forall (r :: RuntimeRep) (a :: TYPE r).
    (forall t s. Alloc# t %One-> State# s %p-> a) %One-> a
runLAST# = \ y -> runST# (runLA# y)