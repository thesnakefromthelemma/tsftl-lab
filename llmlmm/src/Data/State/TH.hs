{-# LANGUAGE Haskell2010
  , CPP
  , GADTSyntax
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskellQuotes
#-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * Verify that 'noPrimOp' is opaque to core and codegens to a no-op

  * Resolve issue #18472, allowing the below FFI imports to be greatly simplified
-}

{-# OPTIONS_GHC -Wall #-}

{- | TemplateHaskell generation of unrestricted-like instances for 'State#' -}
module Data.State.TH
  ( -- * Representation-polymorphic interface to linearly synchronizable types
    declareStatelike
  , declareStatelikeState#
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

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , newName
  , pattern Match
  , pattern CaseE
  , pattern VarE
  , pattern AppTypeE
  , pattern PromotedNilT
  , pattern PromotedT
  , pattern UnboxedTupleT
  , pattern MulArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern BndrReq
  , pattern ForallT
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern NormalB
  , pattern ValD
  , pattern SigD
  , pattern ClassD
  , pattern InstanceD
  , pattern Inline
  , pattern ConLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  , pattern Prim
  , pattern Safe
  , pattern ImportF
  , pattern ForeignD
  , Quote
  )


-- * Representation-polymorphic interface to linearly synchronizable types

-- ** Representation-polymorphic interface to linearly synchronizable types

{-  Morally equivalent to the @CPP@ macro
    @
        #define DECLARE_STATELIKE                             \
        class Statelike (r :: RuntimeRep) (a :: TYPE r) where \
            sync1 :: (# a #) %One-> a                         \
          ; sync2 :: (# a, a #) %One-> a                      \
            ...
          ; sync64 :: (# a, ..., a #) %One-> a
    @
    Requires at least @-XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XTemplateHaskell -XUnboxedTuples@
    (but this is not checked).
-}
declareStatelike :: forall q. Quote q => q Dec
declareStatelike = do
    let statelike_nm = mkName "Statelike"
    r_nm <- newName "r"
    a_nm <- newName "a"
    let rep_n_nm = \ n -> mkName $ "sync" <> show n
    let tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( VarT a_nm )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    let srep_dc = do
#if FULL
            n <- [ 1 .. 64 ]
#else
            n <- [ 1 .. 8 ]
#endif
            [ SigD
              ( rep_n_nm n )
              ( AppT ( AppT ( AppT
                  ( MulArrowT )
                  ( PromotedT 'One ) )
                  ( tup_n_ty n ) )
                  ( VarT a_nm ) ) ]
    pure
      ( ClassD
          [ ]
          ( statelike_nm )
          [ KindedTV
              ( r_nm )
              ( BndrReq )
              ( ConT ''RuntimeRep )
          , KindedTV
              ( a_nm )
              ( BndrReq )
              ( AppT
                  ( ConT ''TYPE )
                  ( VarT r_nm ) ) ]
          [ ]
          ( srep_dc ) )

-- ** TemplateHaskell generation of representation-polymorphic interface to linearly synchronizable types

{- | Auxiliary for 'declareUrlikeState#' -}
data DecType where
    FFI, Inst :: DecType

{- | Generates a @forall t. 'Statelike' ('State#' t)@ instance via prim FFI\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLEAR_STATELIKE_STATE                                              \
        foreign import prim "noPrimOp"                                               \
            sync1_primOp :: forall t. (# State# t #) %One-> State#                   \
        foreign import prim "noPrimOp"                                               \
            sync2_primOp :: forall t. (# State# t, State# t #) %One-> State# t       \
        ...                                                                          \
        foreign import prim "noPrimOp"                                               \
            sync64_primOp :: forall t. (# State# t, ..., State# t #) %One-> State# t \
        instance forall t. Statelike (TYPE (TupleRep '[])) (State# t) where          \
            {-# INLINE sync1 #-}                                                     \
          ; sync1 :: (# State# t #) %One-> State# t                                  \
          ; sync1 = sync1_primOp                                                     \
          ; {-# INLINE rep2 #-}                                                      \
          ; sync2 :: (# State# t, State# t #) %One-> State# t                        \
          ; sync2 = sync2_primOp                                                     \
            ...
          ; {-# INLINE sync64 #-}                                                    \
          ; sync64 :: forall t. (# State# t, ..., State# t #) %One-> State# t        \
          ; sync64 = sync64_primOp
    @
    Requires at least  @-XDataKinds -XFlexibleInstances -XGHCForeignImportPrim -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XScopedTypeVariables -XTemplateHaskell -XTypeApplications -XUnboxedTuples -XUnliftedFFITypes@,
    but this is not checked.
    Requires that @'Urlike' ( .. )@ be in scope,
    but this is not checked.
-}
declareStatelikeState# :: forall q. Quote q => q [Dec]
declareStatelikeState# = sequence $ do
    let statelike_nm = mkName "Statelike"
    let sync_n_primop_nm = \ n -> mkName $ "sync" <> show n <> "_primOp#"
    let tup_n_ty = \ s_nm n -> foldr (\ _ b ->
            AppT
                ( b )
                ( AppT
                    ( ConT ''State# )
                    ( VarT s_nm ) )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    d <-
      [ FFI
      , Inst ]
    case d of
        FFI  -> do
#if FULL
            n <- [ 1 .. 64 ]
#else
            n <- [ 1 .. 8 ]
#endif
            pure $ do
                s_nm <- newName "s"
                pure
                  ( ForeignD ( ImportF
                      ( Prim )
                      ( Safe )
                      ( "noPrimOp" )
                      ( sync_n_primop_nm n )
                      ( ForallT
                          [ KindedTV
                              ( s_nm )
                              ( SpecifiedSpec )
                              ( AppT
                                  ( ConT ''TYPE )
                                  ( AppT
                                      ( PromotedT 'BoxedRep )
                                      ( PromotedT 'Lifted ) ) ) ]
                          [ ]
                          ( AppT ( AppT ( AppT
                              ( MulArrowT )
                              ( PromotedT 'Many ) )
                              ( tup_n_ty s_nm n ) )
                              ( AppT
                                  ( ConT ''State# )
                                  ( VarT s_nm ) ) ) ) ) )
        Inst -> pure $ do
            s_nm <- newName "s"
            let sync_n_nm = \ (n :: Int) -> mkName $ "sync" <> show n
            let srep_dc = do
#if FULL
                    n <- [ 1 .. 64 ]
#else
                    n <- [ 1 .. 8 ]
#endif              
                    id -- just for parser reasons...  
                      [ ValD
                          ( VarP ( sync_n_nm n ) )
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
                                  ( NormalB ( VarE ( sync_n_primop_nm n ) ) )
                                  [ ] ] ) )
                          [ ]
                      , SigD
                          ( sync_n_nm n )
                          ( AppT ( AppT ( AppT
                              ( MulArrowT )
                              ( PromotedT 'One ) )
                              ( tup_n_ty s_nm n ) )
                              ( AppT
                                  ( ConT ''State# )
                                  ( VarT s_nm ) ) )
                      , PragmaD ( InlineP
                          ( sync_n_nm n )
                          ( Inline )
                          ( ConLike )
                          ( AllPhases ) ) ]
            pure
              ( InstanceD
                  ( Nothing )
                  [ ]
                  ( AppT ( AppT
                      ( ConT statelike_nm )
                      ( AppT
                          ( PromotedT 'TupleRep )
                          ( PromotedNilT ) ) )
                      ( AppT
                          ( ConT ''State# )
                          ( VarT s_nm ) ) )
                  ( srep_dc ) )