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

{-# OPTIONS_GHC -Wall #-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * Resolve issue #18472, allowing the below FFI imports to be greatly simplified

  * Add TemplateHaskell support for quantified class instance declarations (cf. GHC-71492)
-}

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

import Control.Monad
  ( join )

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
  , Q
  , pattern FlexibleInstances
  , pattern GHCForeignImportPrim
  , pattern InstanceSigs
  , pattern LinearTypes
  , pattern ScopedTypeVariables
  , pattern TypeApplications
  , pattern UnboxedTuples
  , pattern UnliftedFFITypes
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardValue
  )

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
    Requires @-XFlexibleInstances -XGHCForeignImportPrim -XInstanceSigs -XLinearTypes -XScopedTypeVariables -XTypeApplications -XUnboxedTuples -XUnliftedFFITypes@.
    Requires that @'Prelude.Linear.rep0' .. 'Prelude.Linear.rep64'@ be in scope.
-}
declareUrlikeAlloc# :: Q [Dec]
declareUrlikeAlloc# = join . fmap sequence $ do
    guardExts
      ( "\'Data.State.Linear.declareUrlikeAlloc#\'" )
      [ FlexibleInstances
      , GHCForeignImportPrim
      , InstanceSigs
      , LinearTypes
      , ScopedTypeVariables
      , TypeApplications
      , UnboxedTuples
      , UnliftedFFITypes ]
    let rep_n_primop_nm = \ n -> mkName $ "rep" <> show n <> "_primOp#"
    let tup_n_ty = \ t_nm n -> foldr (\ _ b ->
            AppT
                ( b )
                ( AppT
                    ( ConT ''Alloc# )
                    ( VarT t_nm ) )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    pure $ do
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
                              [ PlainTV
                                  ( t_nm )
                                  ( SpecifiedSpec ) ]
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
                let rep_n_nm_ug = \ n -> "Prelude.Linear.rep" <> show n
                srep_dc <- fmap join . sequence $ do
#if FULL
                    n <- [ 0 .. 64 ]
#else
                    n <- [ 0 .. 8 ]
#endif
                    pure $ do
                        rep_n_nm <- guardValue
                          ( "\'Data.State.Linear.declareUrlikeAlloc#\'" )
                          ( rep_n_nm_ug n )
                        pure -- just for parser reasons...  
                          [ ValD
                              ( VarP ( rep_n_nm ) )
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
                              ( rep_n_nm )
                              ( AppT ( AppT ( AppT
                                  ( MulArrowT )
                                  ( PromotedT 'One ) )
                                  ( AppT
                                      ( ConT ''Alloc# )
                                      ( VarT t_nm ) ) )
                                  ( tup_n_ty t_nm n ) )
                          , PragmaD ( InlineP
                              ( rep_n_nm )
                              ( Inline )
                              ( ConLike )
                              ( AllPhases ) ) ]
                pure
                  ( InstanceD
                      ( Nothing )
                      [ ]
                      {-( ForallT
                          [ PlainTV
                              ( t_nm )
                              ( SpecifiedSpec ) ]
                          [ ]
                          ( AppT
                              ( ConT ''Urlike )
                              ( AppT
                                  ( ConT ''Alloc# )
                                  ( VarT t_nm ) ) ) )-} -- GHC-71492 :(
                      ( AppT
                          ( ConT ''Urlike )
                          ( AppT
                              ( ConT ''Alloc# )
                              ( VarT t_nm ) ) )
                      ( srep_dc ) )