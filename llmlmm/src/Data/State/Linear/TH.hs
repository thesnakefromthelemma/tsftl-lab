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
      ( State# )
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
        Heap
    {- | Linear heap state token -}
    L ::
        Stemma -> -- ^ chain of linear (sub)heap dependencies
        TYPE (BoxedRep Lifted) -> -- ^ shared (sub)heap parameter
        Mutability -> -- ^ writability status
        Nat -> -- ^ immediate (younger sibling + child) dependency count
        Heap

{- | GC/linear (sub)heap 'GHC.State#' tokens -}
type role State# nominal nominal
newtype
    State# ::
        Heap -> -- ^ (sub)heap
        TYPE (BoxedRep Lifted) -> -- ^ unique parameter
        TYPE (TupleRep '[])
    where
    State# ::
        forall (h :: Heap) s.
        GHC.State# s %One->
        State# h s

{- | Given argument @n@,
    generates the running of @n@ @'State#' _@ actions\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLARE_RUN_STATE(N)                                                \
        {-# INLINE CONLIKE runSTN# #-}                                              \
        runSTN# ::                                                                  \
            forall (h1 :: Heap) .. (hN :: Heap) {r :: RuntimeRep} (a :: TYPE r).    \
            (forall s1 .. sN. State# h1 s1 %One-> .. State# hN sN %One-> a) %One->  \
            a                                                                       \
        runSTN# = coerce $ \ x -> runRW# ( .. runRW# x)
    @
    Requires that @N@ be positive.
    Requires @-XDataKinds -XLinearTypes -XMagicHash -XPolyKinds -XRankNTypes -XScopedTypeVariables -XUnboxedTuples@.
    Requires that the constructor 'State#' of 'State#' be in scope.
-}

{- | Given argument @n@,
    generates the synchronization of @n@ @'State#' _@ tokens\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLARE_SYNC_STATE(N)                          \
        foreign import prim "noPrimOp"                         \
            syncN_primOp# ::                                   \
                forall s.                                      \
                (# State# (GC s), .., State# (GC s) #) %Many-> \
                (# State# (GC s), .., State# (GC s) #)         \
        {-# INLINE CONLIKE syncN# #-}                          \
        syncN# ::                                              \
            forall (h1 :: Heap) .. (hN :: Heap) s1 .. sN.      \
            (# State# h1 s1, .., State# hN sN #) %One->        \
            (# State# h1 s1, .., State# hN sN #)               \
        syncN# = case unsafeEqualityProof @Many @One of        \
            UnsafeRefl -> syncN_primOp#
    @
    Requires @-XDataKinds -XGHCForeignImportPrim -XLinearTypes -XMagicHash -XPolyKinds -XScopedTypeVariables -XTypeApplications -XUnboxedTuples -XUnliftedFFITypes@.
    Throws @-Winaccessible-code@ and @-Woverlapping-patterns@.
-}

-- prototypes for (sub)alloc, (sub)free, share, rescind, freeze, thaw

-- existential newtype companions

{- | Given arguments @n_in@, @n_out@,
    generates the forking of @n_out@ 'State# (GC _) _' tokens
    from  @n_in@ 'State#' tokens\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLARE_FORK_STATE_GC(N_IN, N_OUT)                  \
        foreign import prim "noPrimOp"                              \
            forkN_OUTfromN_IN_primOp# ::                            \
                forall s.                                           \
                (# State# (GC s), .., State# (GC s) #) %Many->      \
                (# State# (GC s), .., State# (GC s) #)              \
        {-# INLINE CONLIKE forkN_OUTfromN_IN# #-}                   \
        forkN_OUTfromN_IN# ::                                       \
            forall s.                                               \
            (# State# (GC s), .., State# (GC s) #) %One->           \
            (# State# (GC s), .., State# (GC s) #)                  \
        forkN_OUTfromN_IN# = case unsafeEqualityProof @Many @One of \
            UnsafeRefl -> forkN_OUTfromN_IN_primOp#
    @
    Requires @-XDataKinds -XGHCForeignImportPrim -XLinearTypes -XMagicHash -XScopedTypeVariables -XTypeApplications -XUnboxedTuples -XUnliftedFFITypes@.
    Requires that @N_IN@ be in @[ 1 .. 64 ]@.
    Requires that @N_OUT@ be in @[ 0 .. 64 ]@.
    Throws @-Winaccessible-code@ and @-Woverlapping-patterns@.
-}

{- | Generates a 'Repable' instance for 'State# (GC _)'
    via unsafe linearity coercion\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_REPABLE_STATE_GC(a_ty)              \
        foreign import prim "noPrimOp"                     \
            rep2_primOp ::                                 \
                forall s.                                  \
                State# (GC s) %Many->                      \
                (# State# (GC s), State# (GC s) #)         \
        ..
        foreign import prim "noPrimOp"                     \
            rep64_primOp ::                                \
                forall s.                                  \
                State# (GC s) %Many->                      \
                (# State# (GC s), .., State# (GC s) #)     \
        instance forall s. Repable (State# (GC s)) where   \
          ; {-# INLINE CONLIKE rep2 #-}                    \
          ; rep2 ::                                        \
                forall s.                                  \
                State# (GC s) %One->                       \
                (# State# (GC s), State# (GC s) #)         \
          ; rep2 = case unsafeEqualityProof @Many @One of  \
                UnsafeRefl -> rep2_primOp                  \
            ..
          ; {-# INLINE CONLIKE rep64 #-}                   \
          ; rep64 ::                                       \
                forall s.                                  \
                State# (GC s) %One->                       \
                (# State# (GC s), .., State# (GC s) #)     \
          ; rep64 = case unsafeEqualityProof @Many @One of \
                UnsafeRefl -> rep64_primOp
    @
    Requires @-XDataKinds -XFlexibleInstances -XGHCForeignImportPrim -XLinearTypes -XMagicHash -XScopedTypeVariables -XTypeApplications -XUnboxedTuples -XUnliftedFFITypes@.
    Requires that @'Prelude.Linear.Repable' ( .. )@ be in scope.
    Throws @-Winaccessible-code@ and @-Woverlapping-patterns@.
    Throws @-Worphans@.
-}

-- Remember to derive 'Suppable' for @'State# GC _'@