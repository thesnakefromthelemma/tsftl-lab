{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , GADTSyntax
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , TemplateHaskellQuotes
  , UnliftedNewtypes
#-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * Verify that 'noPrimOp' is opaque to core and codegens to a no-op

  * Resolve issue #18472, allowing the below FFI imports to be greatly simplified
-}

{-# OPTIONS_GHC -Wall #-}

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens\;
    the name of this module is a lie since no TH generators are declared here
-}
module Data.State.Linear.TH
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens
    Alloc#
      ( Alloc# )
    -- * TemplateHaskell generation of @forall t. 'Urlike' ('Alloc#' t)@ instance
  , declareUrlikeAlloc#
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
  , pattern ForallT
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern NormalB
  , pattern ValD
  , pattern SigD
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

-- ++ (internal)

import Prelude.Linear
  ( Urlike )


-- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens -}
newtype Alloc# :: TYPE (BoxedRep Lifted) -> TYPE (TupleRep '[]) where
    Alloc# ::
        forall (t :: TYPE (BoxedRep Lifted)).
        State# t %One-> Alloc# t


-- * TemplateHaskell generation of @forall t. 'Urlike' ('Alloc#' t)@ instance

{- | Auxiliary for 'declareUrlikeAlloc#' -}
data DecType where
    FFI, Inst :: DecType

{- | Generates a @forall t. 'Urlike' ('Alloc#' t)@ instance via prim FFI\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLEAR_URLIKE_ALLOC                                                \
        foreign import prim "noPrimOp"                                              \
            rep0_primOp :: forall t. Alloc# t %One-> (# #)                          \
        foreign import prim "noPrimOp"                                              \
            rep1_primOp :: forall t. Alloc# t %One-> (# Alloc# t #)                 \
        foreign import prim "noPrimOp"                                              \
            rep2_primOp :: forall t. Alloc# t %One-> (# Alloc# t, Alloc# t #)       \
        ...                                                                         \
        foreign import prim "noPrimOp"                                              \
            rep64_primOp :: forall t. Alloc# t %One-> (# Alloc# t, ..., Alloc# t #) \
        instance forall t. Statelike (TYPE (TupleRep '[])) (Alloc# t) where         \
            {-# INLINE rep0 #-}                                                     \
          ; rep0 :: Alloc# t %One-> (# #)                                           \
          ; rep0 = rep0_primOp                                                      \
          ; {-# INLINE rep1 #-}                                                     \
          ; rep1 :: Alloc# t %One-> (# Alloc# t #)                                  \
          ; rep1 = rep1_primOp                                                      \
          ; {-# INLINE rep2 #-}                                                     \
          ; rep2 :: Alloc# t %One-> (# Alloc# t, Alloc# t #)                        \
          ; rep2 = rep2_primOp                                                      \
            ...
          ; {-# INLINE rep64 #-}                                                    \
          ; rep64 :: forall t. Alloc# t %One-> (# Alloc# t, ..., Alloc# t #)        \
          ; rep64 = rep64_primOp
    @
    Requires at least  @-XDataKinds -XFlexibleInstances -XGHCForeignImportPrim -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XScopedTypeVariables -XTemplateHaskell -XTypeApplications -XUnboxedTuples -XUnliftedFFITypes@,
    but this is not checked.
    Requires that @'Urlike' ( .. )@ be in scope,
    but this is not checked.
-}
declareUrlikeAlloc# :: forall q. Quote q => q [Dec]
declareUrlikeAlloc# = sequence $ do
    let rep_n_primop_nm = \ n -> mkName $ "rep" <> show n <> "_primOp#"
    let tup_n_ty = \ t_nm n -> foldr (\ _ b ->
            AppT
                ( b )
                ( AppT
                    ( ConT ''Alloc# )
                    ( VarT t_nm ) )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    d <-
      [ FFI
      , Inst ]
    case d of
        FFI  -> do
#if FULL
            n <- [ 0 .. 64 ]
#else
            n <- [ 0 .. 8 ]
#endif
            pure $ do
                t_nm <- newName "t"
                pure
                  ( ForeignD ( ImportF
                      ( Prim )
                      ( Safe )
                      ( "noPrimOp" )
                      ( rep_n_primop_nm n )
                      ( ForallT
                          [ KindedTV
                              ( t_nm )
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
                              ( AppT
                                  ( ConT ''Alloc# )
                                  ( VarT t_nm ) ) )
                              ( tup_n_ty t_nm n ) ) ) ) )
        Inst -> pure $ do
            t_nm <- newName "t"
            let rep_n_nm = \ (n :: Int) -> mkName $ "rep" <> show n
            let srep_dc = do
#if FULL
                    n <- [ 0 .. 64 ]
#else
                    n <- [ 0 .. 8 ]
#endif              
                    id -- just for parser reasons...  
                      [ ValD
                          ( VarP ( rep_n_nm n ) )
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
                                  ( NormalB ( VarE ( rep_n_primop_nm n ) ) )
                                  [ ] ] ) )
                          [ ]
                      , SigD
                          ( rep_n_nm n )
                          ( AppT ( AppT ( AppT
                              ( MulArrowT )
                              ( PromotedT 'One ) )
                              ( AppT
                                  ( ConT ''Alloc# )
                                  ( VarT t_nm ) ) )
                              ( tup_n_ty t_nm n ) )
                      , PragmaD ( InlineP
                          ( rep_n_nm n )
                          ( Inline )
                          ( ConLike )
                          ( AllPhases ) ) ]
            pure
              ( InstanceD
                  ( Nothing )
                  [ ]
                  ( AppT ( AppT
                      ( ConT ''Urlike )
                      ( AppT
                          ( PromotedT 'TupleRep )
                          ( PromotedNilT ) ) )
                      ( AppT
                          ( ConT ''Alloc# )
                          ( VarT t_nm ) ) )
                  ( srep_dc ) )