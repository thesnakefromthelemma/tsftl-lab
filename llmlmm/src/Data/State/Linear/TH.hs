{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , GADTs
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RoleAnnotations
  , ScopedTypeVariables
  , TemplateHaskellQuotes
  , TypeFamilies
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * The precedence of @`'S'`@,  @`'H'`@, and @`'V'`@ is fine-tuned

  * GHC: Primops can be FFIed as inline

  * ??? FFIs 'Data.State.PrimOps.Cmm.noPrimOp' as an inline primop

  * GHC: The prim FFI supports forall types with fixed underlying representation

  * GHC: The FFI supports linearity annotations (cf. Issue #18472)

  * ??? FFIs 'Data.State.PrimOps.Cmm.noPrimOp' linearly, eliminating coercion and cruft
-}

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens\;
    the name of this module is a lie since no TH generators are declared here
-}
module Data.State.Linear.TH
  ( -- * GC/linear recursive (sub)heaps
    Nat
      ( Z
      , S
      )
  , Mutability
      ( M
      , F
      )
  , Stemma
      ( R
      , H
      , V
      )
  , Heap
      ( GC
      , L
      )
  , State#
      ( StateGC#
      , StateL#
      )
  ) where

-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( pattern Lifted
  , RuntimeRep
      ( TupleRep
      , BoxedRep
      )
  , TYPE
  , pattern One
  , pattern Many
  )

import qualified GHC.Exts as GHC
  ( State# )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , newName
  , pattern Match
  , pattern CaseE
  , pattern VarE
  , pattern AppTypeE
  , pattern PromotedT
  , pattern UnboxedTupleT
  , pattern MulArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern PlainTV
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern NormalB
  , pattern ValD
  , pattern SigD
  , pattern Inline
  , pattern ConLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  , pattern Prim
  , pattern Safe
  , pattern ImportF
  , pattern ForeignD
  , Q
  , pattern DataKinds
  , pattern GADTs
  , pattern GHCForeignImportPrim
  , pattern LinearTypes
  , pattern MagicHash
  , pattern ScopedTypeVariables
  , pattern TypeApplications
  , pattern UnboxedTuples
  , pattern UnliftedDatatypes
  , pattern UnliftedFFITypes
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardNoType
  , guardNoValue
  , guardRange
  )


-- * GC/linear recursive (sub)heaps

{- | Natural numbers -}
infixr 5 `S`
data Nat where
    {- | \"Zero\" -}
    Z :: Nat
    {- | \"Succ\" -}
    S :: Nat -> Nat

{- | Linear (sub)heap writability status -}
data Mutability where
    {- | \"Mutable\" -}
    M :: Mutability
    {- | \"Frozen\" -}
    F :: Mutability

{- | Chains of linear (sub)heap dependencies -}
infixr 5 `H`
infixr 5 `V`
data Stemma :: TYPE (BoxedRep Lifted) where
    {- | Root heap -}
    R :: Stemma
    {- | Shared linear(sub)heap, tagged with elder sibling's unique parameter -}
    H :: TYPE (BoxedRep Lifted) -> Stemma -> Stemma
    {- | Linear sub(sub)heap, tagged with parent's unique parameter -}
    V :: TYPE (BoxedRep Lifted) -> Stemma -> Stemma

{- | (Sub)heaps -}
data Heap where
    {- | Garbage-collected heap state token -}
    GC ::
        TYPE (BoxedRep Lifted) -> -- ^ unique parameter
        Heap
    {- | Linear heap state token -}
    L ::
        Stemma -> -- ^ chain of linear (sub)heap dependencies
        TYPE (BoxedRep Lifted) -> -- ^ shared (sub)heap parameter
        Mutability -> -- ^ writability status
        TYPE (BoxedRep Lifted) -> -- ^ unique parameter
        Nat -> -- ^ immediate (younger sibling + child) dependency count
        Heap

{- | GC/linear (sub)heap 'GHC.State#' tokens -}
data family State# :: Heap -> TYPE (TupleRep '[])
newtype instance
    forall s. State# (GC s)
    where
    StateGC# ::
        forall s.
        GHC.State# s %One->
        State# (GC s)
newtype instance
    forall (h :: Stemma) s0 (m :: Mutability) s (n :: Nat). State# (L h s0 m s n)
    where
    StateL# ::
        forall (h :: Stemma) s0 (m :: Mutability) s (n :: Nat).
        GHC.State# s %One->
        State# (L h s0 m s n)

-- runSTn#

-- prototypes for (sub)alloc, (sub)free, share, rescind, freeze, thaw

-- existential newtype companions

-- Forking for GC State# tokens

-- synchronization for all State# tokens