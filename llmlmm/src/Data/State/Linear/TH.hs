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

  * GHC: Inline primops definable

  * 'declareForkAlloc#' FFIs 'Data.State.PrimOps.Cmm.noPrimOp' as an inline primop

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
  , declareForkAlloc#
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
  , pattern GHCForeignImportPrim
  , pattern LinearTypes
  , pattern MagicHash
  , pattern ScopedTypeVariables
  , pattern TypeApplications
  , pattern UnboxedTuples
  , pattern UnliftedFFITypes
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardRange
  )


-- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear allocation tokens

{- | A usuable allocation token is 'Free'\;
    one that has already allocated is 'Bound'
-}
data Liberty where
    Free :: Liberty
    Bound :: Natural -> Liberty

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

{- | Given argument @n_out@,
    generates the forking of @n_out@ 'Alloc#' tokens
    from a 'Free' alloc token\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLARE_FORK_STATE(N_OUT)                                                    \
        foreign import prim "noPrimOp"                                                       \
            forkN_OUT_primOp# ::                                                             \
                forall (l :: Liberty) t0 t1 .. tN_OUT.                                       \
                Alloc# l t0 %One-> (# Alloc l t0, Alloc# Free t1, ..., Alloc# Free tN_OUT #) \
            forkN_OUT# ::                                                                    \
                forall (l :: Liberty) t0 t1 .. tN_OUT.                                       \
                Alloc# l t0 %One-> (# Alloc l t0, Alloc# Free t1, ..., Alloc# Free tN_OUT #) \
            forkN_OUT# = case unsafeEqualityProof @Many @One of                              \
                UnsafeRefl -> forkN_OUT_primOp#
    @
    Requires @-XDataKinds -XGHCForeignImportPrim -XLinearTypes -XMagicHash -XScopedTypeVariables -XTypeApplications -XUnboxedTuples -XUnliftedFFITypes@.
    Requires that @N_OUT@ be in @[ 0 .. 63 ]@.    
-}
declareForkAlloc# :: Int -> Q [Dec]
declareForkAlloc# = \ n -> do
    guardExts
      ( "\'Data.State.declareForkAlloc#\'" )
      [ DataKinds
      , GHCForeignImportPrim
      , LinearTypes
      , MagicHash
      , ScopedTypeVariables
      , TypeApplications
      , UnboxedTuples
      , UnliftedFFITypes ]
    guardRange
      ( "\'Data.State.Linear.declareForkAlloc#\'" )
      ( "@n_out@" )
      ( 0 )
      ( 63 )
      ( n )
    l_nm <- newName "l" -- not great to be recycling these...
    t0_nm <- newName "t0"
    let k_ty =
          ( AppT
              ( UnboxedTupleT (n+1) )
              ( AppT ( AppT
                  ( ConT ''Alloc# )
                  ( VarT l_nm ) )
                  ( VarT t0_nm ) ) )
    (st_tv, tup_n_ty) <- foldr (\ _ k st_tv' -> do
        t_nm <- newName "t"
        let t_tv =
              ( PlainTV
                  ( t_nm )
                  ( SpecifiedSpec ) )
        ~(st_tv'', tup_n_ty') <- k (t_tv : st_tv')
        pure
          ( st_tv''
          , AppT
              ( tup_n_ty' )
              ( AppT ( AppT
                  ( ConT ''Alloc#  )
                  ( VarT l_nm ) )
                  ( VarT t_nm ) ) )
      ) (\ st_tv' -> pure (st_tv', k_ty)) [ n, n-1 .. 1 ] [ ]
    let stv =
          [ KindedTV
              ( l_nm )
              ( SpecifiedSpec )
              ( ConT ''Liberty )
          , PlainTV
              ( t0_nm )
              ( SpecifiedSpec ) ]
          ++ st_tv
    let fork_n_primop_nm = mkName $ "fork" <> show n <> "_primOp#"
    let fork_n_nm = mkName $ "fork" <> show n <> "#"
    pure
      [ ForeignD ( ImportF
          ( Prim )
          ( Safe )
          ( "noPrimOp" )
          ( fork_n_primop_nm )
          ( ForallT
              ( stv )
              [ ]
              ( AppT ( AppT ( AppT
                  ( MulArrowT )
                  ( PromotedT 'Many ) )
                  ( AppT ( AppT
                      ( ConT ''Alloc# )
                      ( VarT l_nm ) )
                      ( VarT t0_nm ) ) )
                  ( tup_n_ty ) ) ) )
      , ValD
          ( VarP ( fork_n_nm ) )
          ( NormalB ( CaseE
              ( AppTypeE ( AppTypeE
                  ( VarE 'unsafeEqualityProof )
                  ( PromotedT 'Many ) )
                  ( PromotedT 'One ) )
              [ Match
                  ( ConP
                      ( 'UnsafeRefl )
                      [ ]
                      [ ] )
                  ( NormalB ( VarE fork_n_primop_nm ) )
                  [ ] ] ) )
          [ ]
      , SigD
          ( fork_n_nm )
          ( ForallT
              ( stv )
              [ ]
              ( AppT ( AppT ( AppT
                  ( MulArrowT )
                  ( PromotedT 'One ) )
                  ( AppT ( AppT
                      ( ConT ''Alloc# )
                      ( VarT l_nm ) )
                      ( VarT t0_nm ) ) )
                  ( tup_n_ty ) ) )
      , PragmaD ( InlineP
          ( fork_n_nm )
          ( Inline )
          ( ConLike )
          ( AllPhases ) ) ]

--  synchronization primitives for Alloc# (Bound _) t (same type)

-- runLAn#