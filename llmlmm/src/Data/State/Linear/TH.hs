{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , GADTSyntax
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

  * GHC: 'noPrimOp' defined as inline primop

  * 'declareForkAlloc#' FFIs 'Data.State.PrimOps.Cmm.noPrimOp' as an inline primop

  * GHC: The prim FFI supports forall types with fixed underlying representation

  * GHC: The FFI supports linearity annotations (cf. Issue #18472)

  * 'declareForkAlloc#' FFIs 'Data.State.PrimOps.Cmm.noPrimOp' linearly, eliminating coercion and cruft
-}

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens\;
    the name of this module is a lie since no TH generators are declared here
-}
module Data.State.Linear.TH
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens
    Liberty
      ( Free
      , Bound
      )
  , Alloc#
      ( Alloc# )
    -- * TemplateHaskell generation of @forall t. 'Urlike' ('Alloc#' t)@ instance
    -- ??
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
  , State#
  , pattern One
  , pattern Many
  )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

import GHC.TypeNats
  ( Natural )

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


-- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear allocation tokens

{- | A usuable allocation token is 'Free'\;
    one that has already allocated is 'Bound'
-}
data Liberty where
    Free :: Liberty
    Bound :: Maybe (TYPE (BoxedRep Lifted)) -> Natural -> Liberty

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens -}
type role Alloc# nominal nominal
newtype
    Alloc# ::
        Liberty -> TYPE (BoxedRep Lifted) -> TYPE (TupleRep '[])
    where
    Alloc# ::
        forall (l :: Liberty) (t :: TYPE (BoxedRep Lifted)) .
        State# t %One-> Alloc# l t


-- * TemplateHaskell generation of 'Alloc#' token forking

-- ???

-- Forking Bounds

--  synchronization primitives for Alloc# (Bound _) t (same type)

-- runLAn#

-- existential newtypes