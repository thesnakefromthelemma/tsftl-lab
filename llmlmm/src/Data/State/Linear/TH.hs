{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , GADTSyntax
  , InstanceSigs
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

  * Verify that 'repPrimOp' is opaque to core and codegens to a no-op
-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
#-}

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens\;
    the name of this module is a lie since no TH generators are declared here
-}
module Data.State.Linear.TH
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens
    LAlloc#
      ( LAlloc# )
    -- * TemplateHaskell generation of @forall t. 'Urlike' ('LAlloc#' t)@ instance
  , declareUrlikeLAlloc#
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
  ( Urlike
      ( .. )
  )


-- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized linear state tokens -}
newtype LAlloc# :: TYPE (BoxedRep Lifted) -> TYPE (TupleRep '[]) where
    LAlloc# ::
        forall (t :: TYPE (BoxedRep Lifted)).
        State# t %One-> LAlloc# t


-- * TemplateHaskell generation of @forall t. 'Urlike' ('LAlloc#' t)@ instance

{- | Auxiliary for 'declareUrlikeLAlloc#' -}
data DecType where
    FFI, Inst :: DecType

{- | Generates a @forall t. 'Urlike' ('LAlloc#' t)@ instance via prim FFI\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLEAR_URLIKE_UR(r)                                                \
        instance forall (a :: TYPE r). Urlike (TYPE (BoxedRep Lifted)) (Ur a) where \
            {-# INLINE rep0 #-}                                                     \
          ; rep0 :: Ur a %One-> (# #)                                               \
          ; rep0 = evUr (\ _ -> (# #))                                              \
          ; {-# INLINE rep1 #-}                                                     \
          ; rep1 :: Ur a %One-> (# Ur a #)                                          \
          ; rep1 = \ ua -> (# ua #)                                                 \
          ; {-# INLINE rep2 #-}                                                     \
          ; rep2 :: Ur a %One-> (# Ur a, Ur a #)                                    \
          ; rep2 = evUr (\ a -> (# ur a, ur a #))                                   \
            ...
          ; {-# INLINE rep64 #-}                                                    \
          ; rep64 :: Ur a %One-> (# Ur a, ..., Ur a #)                              \
          ; rep64 = evUr (\ a -> (# ur a, ..., ur a #))
    @
    Requires at least  @-XDataKinds -XFlexibleInstances -XGHCForeignImportPrim -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XScopedTypeVariables -XTemplateHaskell -XUnboxedTuples -XUnliftedFFITypes@,
    but this is not checked.
    Requires that @'Urlike' ( .. )@ be in scope,
    but this is not checked.
-}
declareUrlikeLAlloc# :: forall q. Quote q => q [Dec]
declareUrlikeLAlloc# = sequence $ do
    let rep_n_primop_nm = \ n -> mkName $ "rep" <> show n <> "_primOp#"
    let tup_n_ty = \ t_nm n -> foldr (\ _ b ->
            AppT
                ( b )
                ( AppT
                    ( ConT ''LAlloc# )
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
                      ( "repPrimOp" )
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
                                  ( ConT ''LAlloc# )
                                  ( VarT t_nm ) ) )
                              ( tup_n_ty t_nm n ) ) ) ) )
        Inst -> pure $ do
            t_nm <- newName "t"
            let rep_n_nm = \ (n :: Int) -> mkName $ "rep" <> show n
            let s_rep_dc = do
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
                                  ( ConT ''LAlloc# )
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
                          ( ConT ''LAlloc# )
                          ( VarT t_nm ) ) )
                  ( s_rep_dc ) )