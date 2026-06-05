{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , LambdaCase
  , LinearTypes
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , StandaloneKindSignatures
  , TemplateHaskellQuotes
  , TypeFamilies
#-}

{-# OPTIONS_GHC -Wall #-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~
  * GHC: TemplateHaskell supports constructor multiplicity (cf. GHC-65904)

  * 'deriveUrable' explictly declares constructor multiplicity of @Ur*@

  * GHC: TemplateHaskell supports quantified class instance declarations (cf. GHC-71492)

  * 'declareRepableUr' explicitly quantifies class instance declaration

  * 'declareSuppableUr' explicitly quantifies class instance declaration
-}

{- | TemplateHaskell generation of unrestricted-related utilities/interfaces -}
module Prelude.Linear.TH
  ( -- * Representation-polymorphic interface to strict unrestricted modality
    Urable
      ( Ur
      , ur
      , evUr
      )
  , deriveUrable
    -- * Representation-polymorphic interface to unrestricted-like types
  , declareRepable
  , declareRepableUnit
  , declareRepableUr
  , deriveRepable
  , Suppable
      ( supp )
  , declareSuppableUnit
  , declareSuppableUr
  , deriveSuppable
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( pattern Lifted
  , pattern Unlifted
  , RuntimeRep
      ( BoxedRep )
  , TYPE
  , pattern One
  , pattern Many
  , Constraint
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
  , pattern UnboxedTupE
  , pattern ConE
  , pattern Match
  , pattern CaseE
  , pattern AppE
  , pattern VarE
  , pattern LamE
  , pattern AppTypeE
  , Type
  , pattern PromotedT
  , pattern UnboxedTupleT
  , pattern ArrowT
  , pattern MulArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern InferredSpec
  , pattern BndrReq
  , pattern ForallT
  , pattern SigT
  , pattern WildP
  , pattern UnboxedTupP
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern NormalB
  , pattern ValD
  , pattern SigD
  , pattern KiSigD
  , pattern NoSourceUnpackedness
  , pattern SourceUnpack
  , pattern SourceStrict
  , pattern Bang
  , pattern GadtC
  , pattern ForallC
  , pattern ClassD
  , pattern InstanceD
  , pattern DataInstD
  , pattern Inline
  , pattern ConLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  , Q
  , pattern DataKinds
  , pattern FlexibleInstances
  , pattern GADTSyntax
  , pattern InstanceSigs
  , pattern LinearTypes
  , pattern MagicHash
  , pattern MultiParamTypeClasses
  , pattern PolyKinds
  , pattern ScopedTypeVariables
  , pattern StandaloneKindSignatures
  , pattern TupleSections
  , pattern TypeApplications
  , pattern TypeFamilies
  , pattern UnboxedTuples
  , pattern UnliftedDatatypes
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardValue
  , guardType
  , guardNoInstance
  )

import Data.RuntimeRep
  ( pattern Prim
  , repGrp
  , repType
  , repStem
  )


-- * Interface to representation-polymorphic strict unrestricted modality

-- ** Interface to representation-polymorphic strict unrestricted modality

{- | Interface to representation-polymorphic strict unrestricted modality -}
class Urable (r :: RuntimeRep) where
    data Ur ::
        TYPE r -> TYPE (BoxedRep Unlifted)
    ur ::
        forall (a :: TYPE r).
        a %Many-> Ur a
    evUr ::
        forall (a :: TYPE r) {s :: RuntimeRep} (b :: TYPE s).
        (a %Many-> b) %1 -> Ur a %1 -> b

-- ** TemplateHaskell generation of strict unrestricted modalities via unsafe linearity coercion

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates a strict unrestricted modality ('Ur') instance
    for types of the representation corresponding to @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_URABLE(r, cn_nm)                                    \
        instance Urable (r) where                                          \
            data instance Ur :: TYPE (r) -> TYPE (BoxedRep Unlifted) where \
                cn_nm ::                                                   \
                    forall (a :: TYPE (r)).                                \
                    {-# UNPACK #-} !a %Many-> Ur a                         \
          ; {-# INLINE CONLIKE ur #-}                                      \
          ; ur ::                                                          \
                forall (a :: TYPE (r)).                                    \
                a %Many-> Ur a                                             \
          ; ur = cn_nm                                                     \
          ; {-# INLINE CONLIKE evUr #-}                                    \
          ; evUr ::                                                        \
                forall (a :: TYPE (r)) {s :: RuntimeRep} (b :: TYPE s).    \
                (a %Many-> b) %One-> Ur a %One-> b                         \
          ; evUr = \ f (cn_nm a) -> f a
    @
    Requires @-XDataKinds -XGADTSyntax -XInstanceSigs -XLinearTypes -XPolyKinds -XScopedTypeVariables -XTypeFamilies -XUnliftedDatatypes@\;
    if @repGrp r@ is not 'Prim' then requires @-XFlexibleInstances@\;
    if @r@ is not 'BoxedRep Lifted' then requires @-XMagicHash@.
    Throws @-Worphans@.
-}
deriveUrable :: RuntimeRep -> Q Dec
deriveUrable = \ r -> do
    guardExts
      ( "\'Prelude.Linear.deriveUrable\'" )
      [ DataKinds
      , GADTSyntax
      , InstanceSigs
      , LinearTypes
      , PolyKinds
      , ScopedTypeVariables
      , TypeFamilies
      , UnliftedDatatypes ]
    case repGrp r of
        Prim -> pure ()
        _    -> guardExts
          ( "@Prelude.Linear.deriveUrable (" <> show r <> ")@" )
          [ FlexibleInstances ]
    case r of
        BoxedRep Lifted -> pure ()
        _               -> guardExts
          ( "@Prelude.Linear.deriveUrable (" <> show r <> ")@" )
          [ MagicHash ]
    let r_ty = repType r
    guardNoInstance
      ( "@Prelude.Linear.deriveUrable (" <> show r <> ")@" )
      ( ''Urable )
      [ r_ty ]
    let cn_nm = mkName $ "Ur" <> repStem r <> case r of BoxedRep Lifted -> ""; _ -> "#"
    let cn_up = case r of
            BoxedRep _ -> NoSourceUnpackedness
            _          -> SourceUnpack
    a_ty_nm <- newName "a"
    f_nm <- newName "f"
    a_ex_nm <- newName "a"
    s_nm <- newName "s"
    b_nm <- newName "b"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT
              ( ConT ''Urable )
              ( r_ty ) )
          [ DataInstD
              [ ]
              ( Nothing )
              ( ConT ''Ur )
              ( Just ( AppT ( AppT
                  ( ArrowT )
                  ( AppT
                      ( ConT ''TYPE )
                      ( r_ty ) ) )
                  ( AppT
                      ( ConT ''TYPE )
                      ( AppT
                          ( PromotedT 'BoxedRep )
                          ( PromotedT 'Unlifted ) ) ) ) )
              [ ForallC
                  [ KindedTV
                      ( a_ty_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( r_ty ) ) ]
                  [ ]
                  ( GadtC
                      [ cn_nm ]
                      [ ( Bang cn_up SourceStrict -- WARNING: We haven't specified a linearity\; cf. GHC-65904
                        , VarT a_ty_nm ) ]
                      ( AppT
                          ( ConT ''Ur )
                          ( VarT a_ty_nm ) ) ) ]
              [ ]
          , ValD
              ( VarP 'ur )
              ( NormalB ( ConE cn_nm ) )
              [ ]
          , SigD
              ( 'ur )
              ( ForallT
                  [ KindedTV
                      ( a_ty_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( r_ty ) ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'Many ) )
                      ( VarT a_ty_nm ) )
                      ( AppT
                          ( ConT ''Ur )
                          ( VarT a_ty_nm ) ) ) )
          , PragmaD ( InlineP
              ( 'ur )
              ( Inline )
              ( ConLike )
              ( AllPhases ) )
          , ValD
              ( VarP 'evUr )
              ( NormalB ( LamE
                  [ VarP f_nm
                  , ConP
                      ( cn_nm )
                      [ ]
                      [ VarP a_ex_nm ] ]
                  ( AppE
                      ( VarE f_nm )
                      ( VarE a_ex_nm ) ) ) )
              [ ]
          , SigD
              ( 'evUr )
              ( ForallT
                  [ KindedTV
                      ( a_ty_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( r_ty ) )
                  , KindedTV
                      ( s_nm )
                      ( InferredSpec )
                      ( ConT ''RuntimeRep )
                  , KindedTV
                      ( b_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( VarT s_nm ) ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'Many) )
                          ( VarT a_ty_nm ) )
                          ( VarT b_nm ) ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( AppT
                              ( ConT ''Ur )
                              ( VarT a_ty_nm ) ) )
                          ( VarT b_nm ) ) ) )
          , PragmaD ( InlineP
              ( 'evUr )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )


-- * Representation-polymorphic interface to unrestricted-like types

-- ** TemplateHaskell generation of representation-polymorphic interface to linearly replicable types

{-  Morally equivalent to the code
    @
        type Repable ::
            forall {r :: RuntimeRep}. TYPE r -> Constraint
        class Repable a where
            rep2 :: a %One-> (# a, a #)
            ...
            rep64 :: a %One-> (# a, ..., a #)
    @
    Requires @-XLinearTypes -XPolyKinds -XStandaloneKindSignatures -XUnboxedTuples@.
-}
declareRepable :: Q [Dec]
declareRepable = do
    guardExts
      ( "\'Prelude.Linear.declareRepable\'" )
      [ LinearTypes
      , PolyKinds
      , StandaloneKindSignatures
      , UnboxedTuples ]
    let repable_nm = mkName "Repable"
    r_nm <- newName "r"
    a_nm <- newName "a"
    let rep_n_nm = \ n -> mkName $ "rep" <> show n
    let tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( VarT a_nm )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    let srep_dc = do
#if FULL
            n <- [ 2 .. 64 ]
#else
            n <- [ 2 .. 8 ]
#endif
            [ SigD
              ( rep_n_nm n )
              ( AppT ( AppT ( AppT
                  ( MulArrowT )
                  ( PromotedT 'One ) )
                  ( VarT a_nm ) )
                  ( tup_n_ty n ) ) ]
    pure
      [ KiSigD
          ( repable_nm )
          ( ForallT
              [ KindedTV
                  ( r_nm )
                  ( InferredSpec )
                  ( ConT ''RuntimeRep ) ]
              [ ]
              ( AppT ( AppT
                  ( ArrowT )
                  ( AppT
                      ( ConT ''TYPE )
                      ( VarT r_nm ) ) )
                  ( ConT ''Constraint ) ) )
      , ClassD
          [ ]
          ( repable_nm )
          [ KindedTV
              ( a_nm )
              ( BndrReq )
              ( AppT
                  ( ConT ''TYPE )
                  ( VarT r_nm ) ) ]
          [ ]
          ( srep_dc ) ]

-- ** TemplateHaskell generation of representation-polymorphic linearly replicable instances

{- | Generates a 'Repable' instance for @(# #)@\;
    morally equivalent to the code
    @
        instance Repable (# #) where
            {-# INLINE CONLIKE rep2 #-}
            rep2 :: (# #) %One-> (# (# #), (# #) #)
            rep2 = \ (# #) -> (# (# #), (# #) #)
            ...
            {-# INLINE CONLIKE rep64 #-}
            rep64 :: (# #) %One-> (# (# #), ..., (# #) #)
            rep64 = \ (# #) -> (# (# #), ..., (# #) #)
    @
    Requires @-XDataKinds -XInstanceSigs -XLinearTypes -XUnboxedTuples@.
    Requires that @'Prelude.Linear.Repable' (..)@ be in scope.
    Potentially throws @-Worphans@.
-}
declareRepableUnit :: Q Dec
declareRepableUnit = do
    guardExts
      ( "\'Prelude.Linear.declareRepableUnit\'" )
      [ DataKinds
      , InstanceSigs
      , LinearTypes
      , UnboxedTuples ]
    repable_nm <- guardType
      ( "\'Prelude.Linear.declareRepableUnit\'" )
      ( "Prelude.Linear.Repable" )
    guardNoInstance
      ( "\'Prelude.Linear.declareRepableUnit\'" )
      ( repable_nm )
      [ UnboxedTupleT 0 ]
    let rep_n_nm_ug = \ n -> "Prelude.Linear.rep" <> show n
    let tup_n_ex = \ n -> do
            (_ :: Int) <- [ 0 .. n - 1 ]
            [ Just ( UnboxedTupE [ ] ) ]
    let tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( UnboxedTupleT 0 )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    srep_dec <- fmap join . sequence $ do
#if FULL
            n <- [ 2 .. 64 ]
#else
            n <- [ 2 .. 8 ]
#endif
            pure $ do
                rep_n_nm <- guardValue
                  ( "\'Prelude.Linear.declareRepableUnit\'" )
                  ( rep_n_nm_ug n )
                pure
                  [ ValD
                      ( VarP ( rep_n_nm ) )
                      ( NormalB ( ( LamE
                          [ UnboxedTupP [ ] ]
                          ( UnboxedTupE ( tup_n_ex n ) ) ) ) )
                      [ ]
                  , SigD
                      ( rep_n_nm )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( UnboxedTupleT 0 ) )
                          ( tup_n_ty n ) )
                  , PragmaD ( InlineP
                      ( rep_n_nm )
                      ( Inline )
                      ( ConLike )
                      ( AllPhases ) ) ]
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT
              ( ConT repable_nm )
              ( UnboxedTupleT 0 ) )
          ( srep_dec ) )

{- | Given argument @r@,
    generates a 'Repable' instance
    for all @Ur a@ with @a@ a type of representation @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLEAR_REPABLE_UR(r)                       \
        instance forall (a :: TYPE r). Repable (Ur a) where \
            {-# INLINE CONLIKE rep2 #-}                     \
          ; rep2 :: Ur a %One-> (# Ur a, Ur a #)            \
          ; rep2 = evUr (\ a -> (# ur a, ur a #))           \
            ...
          ; {-# INLINE CONLIKE rep64 #-}                    \
          ; rep64 :: Ur a %One-> (# Ur a, ..., Ur a #)      \
          ; rep64 = evUr (\ a -> (# ur a, ..., ur a #))
    @
    Requires @-XDataKinds -XFlexibleInstances -XInstanceSigs -XLinearTypes -XPolyKinds -XUnboxedTuples@.
    Requires that @'Prelude.Linear.Repable' (..)@ be in scope.
    Potentially throws @-Worphans@.
-}
declareRepableUr :: RuntimeRep -> Q Dec
declareRepableUr = \ r -> do
    guardExts
      ( "\'Prelude.Linear.declareRepableUr\'" )
      [ DataKinds
      , FlexibleInstances
      , InstanceSigs
      , LinearTypes
      , PolyKinds
      , UnboxedTuples ]
    repable_nm <- guardType
      ( "\'Prelude.Linear.declareRepableUr\'" )
      ( "Prelude.Linear.Repable" )
    let r_ty = repType r
    a_ty_nm <- newName "a"
    guardNoInstance
      ( "@Prelude.Linear.declareUrlikeUr (" <> show r <> ")@" )
      ( repable_nm )
      [ AppT
          ( ConT ''Ur )
          ( SigT
              ( VarT a_ty_nm )
              ( AppT
                  ( ConT ''TYPE )
                  ( r_ty ) ) ) ]
    a_ex_nm <- newName "a"
    let rep_n_nm_ug = \ n -> "Prelude.Linear.rep" <> show n
    let stup_n_ex = \ n -> do
            (_ :: Int) <- [ 0 .. n - 1 ]
            [ Just ( AppE
                ( VarE 'ur )
                ( VarE a_ex_nm ) ) ]
    let tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( AppT
                  ( ConT ''Ur )
                  ( VarT a_ty_nm ) )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    srep_dec <- fmap join . sequence $ do
#if FULL
        n <- [ 2 .. 64 ]
#else
        n <- [ 2 .. 8 ]
#endif
        pure $ do
            rep_n_nm <- guardValue
              ( "\'Prelude.Linear.declareUrlikeUr\'" )
              ( rep_n_nm_ug n )
            pure
              [ ValD
                  ( VarP ( rep_n_nm ) )
                  ( NormalB ( AppE
                      ( VarE 'evUr )
                      ( LamE
                          [ VarP a_ex_nm ]
                          ( UnboxedTupE ( stup_n_ex n ) ) ) ) )
                  [ ]
              , SigD
                  ( rep_n_nm )
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''Ur )
                          ( VarT a_ty_nm ) ) )
                      ( tup_n_ty n ) )
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
              [ KindedTV
                  ( a_ty_nm )
                  ( SpecifiedSpec )
                  ( AppT
                      ( ConT ''TYPE )
                      ( r_ty ) ) ]
              [ ]
              ( AppT
                  ( ConT repable_nm )
                  ( AppT
                      ( ConT ''Ur )
                      ( VarT a_ty_nm ) ) ) )-} -- GHC-71492 :(
          ( AppT
              ( ConT repable_nm )
              ( AppT
                  ( ConT ''Ur )
                  ( SigT
                      ( VarT a_ty_nm )
                      ( AppT
                          ( ConT ''TYPE )
                          ( r_ty ) ) ) ) )
          ( srep_dec ) )

{- | Given argument @a_ty@,
    reprsenting a type (of any representation),
    generates an 'Repable' instance for the latter
    via unsafe linearity coercion\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_REPABLE(a_ty)                       \
        instance Urlike (a_ty) where                       \
            {-# INLINE CONLIKE rep2 #-}                    \
          ; rep2 :: a %One-> (# a, a #)                    \
          ; rep2 = case unsafeEqualityProof @Many @One of  \
                UnsafeRefl -> \ a -> (# a, a #)            \
            ...
          ; {-# INLINE CONLIKE rep64 #-}                   \
          ; rep64 :: a %One-> (# a, ..., a #)              \
          ; rep64 = case unsafeEqualityProof @Many @One of \
                UnsafeRefl -> \ a -> (# a, ..., a #)
    @
    Requires @-XDataKinds -XInstanceSigs -XLinearTypes -XTupleSections -XTypeApplications -XUnboxedTuples@.
    Requires that @'Prelude.Linear.Repable' ( .. )@ be in scope.
    Throws @-Winaccessible-code@ and @-Woverlapping-patterns@.
    Potentially throws @-Worphans@.
-}
deriveRepable :: Type -> Q Dec
deriveRepable = \ a_ty -> do
    guardExts
      ( "\'Prelude.Linear.deriveRepable\'" )
      [ DataKinds
      , InstanceSigs
      , LinearTypes
      , TupleSections
      , TypeApplications
      , UnboxedTuples ]
    repable_nm <- guardType
      ( "\'Prelude.Linear.deriveRepable\'" )
      ( "Prelude.Linear.Repable" )
    guardNoInstance
      ( "@Prelude.Linear.deriveRepable (" <> show a_ty <> ")@" )
      ( repable_nm )
      [ a_ty ]
    a_ex_nm <- newName "a"
    let rep_n_nm_ug = \ n -> "Prelude.Linear.rep" <> show n
    let stup_n_ex = \ n -> do
            (_ :: Int) <- [ 0 .. n - 1 ]
            [ Just ( VarE a_ex_nm ) ]
    let tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( a_ty )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    srep_dec <- fmap join . sequence $ do
#if FULL
        n <- [ 2 .. 64 ]
#else
        n <- [ 2 .. 8 ]
#endif
        pure $ do
            rep_n_nm <- guardValue
              ( "\'Prelude.Linear.deriveRepable\'" )
              ( rep_n_nm_ug n )
            pure
              [ ValD
                  ( VarP ( rep_n_nm ) )
                  ( NormalB ( ( CaseE
                      ( AppTypeE ( AppTypeE
                          ( VarE 'unsafeEqualityProof )
                          ( PromotedT 'Many ) )
                          ( PromotedT 'One ) )
                      [ Match
                          ( ConP
                              ( 'UnsafeRefl )
                              [ ]
                              [ ] )
                          ( NormalB ( LamE
                              [ VarP a_ex_nm ]
                              ( UnboxedTupE ( stup_n_ex n ) ) ) )
                          [ ] ] ) ) )
                      [ ]
              , SigD
                  ( rep_n_nm )
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( a_ty ) )
                      ( tup_n_ty n ) )
              , PragmaD ( InlineP
                  ( rep_n_nm )
                  ( Inline )
                  ( ConLike )
                  ( AllPhases ) ) ]
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT
              ( ConT repable_nm )
              ( a_ty ) )
          ( srep_dec ) )


-- * Representation-polymorphic interface to linearly suppressible types

-- ** Representation-polymorphic interface to linearly suppressible types

type Suppable ::
    forall {r :: RuntimeRep}. TYPE r -> RuntimeRep -> Constraint
class Suppable a (s :: RuntimeRep) where
    infixr 0 `supp`
    supp :: forall (b :: TYPE s). a %One-> b %One-> b

-- ** TemplateHaskell generation of linearly suppressible instances

{- | Given argument @s@,
    generates a @'Suppable' (# #) s@ instance;
    morally equivalent to the CPP macro
    @
        #define DECLARE_SUPPABLE_UNIT(s)                            \
        instance Suppable (# #) (s) where                           \
            {-# INLINE CONLIKE supp #-}                             \
          ; supp :: forall (b :: TYPE (s)). (# #) %One-> b %One-> b \
          ; supp = \ (# #) b -> b
    @
    Requires @-XDataKinds -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XScopedTypeVariables -XUnboxedTuples@;
    if @repGrp s@ is not 'Prim' then requires @-XFlexibleInstances@.
    Requires that @'Prelude.Linear.Repable' (..)@ be in scope.
    Potentially throws @-Worphans@.
-}
declareSuppableUnit :: RuntimeRep -> Q Dec
declareSuppableUnit = \ s -> do
    guardExts
      ( "\'Prelude.Linear.declareSuppableUnit\'" )
      [ DataKinds
      , InstanceSigs
      , LinearTypes
      , MultiParamTypeClasses
      , PolyKinds
      , ScopedTypeVariables
      , UnboxedTuples ]
    case repGrp s of
        Prim -> pure ()
        _    -> guardExts
          ( "@Prelude.Linear.declareSuppableUnit _ (" <> show s <> ")@" )
          [ FlexibleInstances ]
    let s_ty = repType s
    guardNoInstance
      ( "@Prelude.Linear.declareSuppableUnit (# #) (" <> show s <> ")@" )
      ( ''Suppable )
      [ UnboxedTupleT 0
      , s_ty ]    
    b_ty_nm <- newName "b"
    b_ex_nm <- newName "b"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT ( AppT
              ( ConT ''Suppable )
              ( UnboxedTupleT 0 ) )
              ( s_ty ) )
          [ ValD
              ( VarP ( 'supp ) )
              ( NormalB ( LamE
                  [ UnboxedTupP [ ]
                  , VarP b_ex_nm ]
                  ( VarE b_ex_nm ) ) )
              [ ]
          , SigD
              ( 'supp )
              ( ForallT
                  [ KindedTV
                      ( b_ty_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( s_ty ) ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( UnboxedTupleT 0 ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( VarT b_ty_nm ) )
                          ( VarT b_ty_nm ) ) ) )
          , PragmaD ( InlineP
              ( 'supp )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )

{- | Given argument @s@,
    generates a @'Suppable' (# #) s@ instance;
    morally equivalent to the CPP macro
    @
        #define DECLARE_SUPPABLE_UR(r, s)                          \
        instance forall (a :: TYPE (r)). Suppable (Ur a) (s) where \
            {-# INLINE CONLIKE supp #-}                            \
          ; supp :: forall (b :: TYPE (s)). Ur a %One-> b %One-> b \
          ; supp = evUr (\ _ b -> b)
    @
    Requires @-XDataKinds -XFlexibleInstances -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XScopedTypeVariables -XUnboxedTuples@.
    Requires that @'Prelude.Linear.Repable' (..)@ be in scope.
    Potentially throws @-Worphans@.
-}
declareSuppableUr :: RuntimeRep -> RuntimeRep -> Q Dec
declareSuppableUr = \ r s -> do
    guardExts
      ( "\'Prelude.Linear.declareSuppableUr\'" )
      [ DataKinds
      , FlexibleInstances
      , InstanceSigs
      , LinearTypes
      , MultiParamTypeClasses
      , PolyKinds
      , ScopedTypeVariables
      , UnboxedTuples ]
    let r_ty = repType r
    a_nm <- newName "a"
    let s_ty = repType s
    guardNoInstance
      ( "@Prelude.Linear.declareSuppableUr (" <> show r <> ") (" <> show s <> ")@" )
      ( ''Suppable )
      [ AppT
          ( ConT ''Ur )
          ( SigT
              ( VarT a_nm )
              ( AppT
                  ( ConT ''TYPE )
                  ( r_ty ) ) )
      , s_ty ]    
    b_ty_nm <- newName "b"
    b_ex_nm <- newName "b"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          {-( ForallT
              [ KindedTV
                  ( a_nm )
                  ( SpecifiedSpec )
                  ( AppT
                      ( ConT ''TYPE )
                      ( r_ty ) ) ]
              [ ]
              ( AppT
                  ( ConT ''Suppable )
                  ( AppT
                      ( ConT ''Ur )
                      ( VarT a_nm ) ) ) )-} -- GHC-71492 :(
          ( AppT ( AppT
              ( ConT ''Suppable )
              ( AppT
                  ( ConT ''Ur )
                  ( SigT
                      ( VarT a_nm )
                      ( AppT
                          ( ConT ''TYPE )
                          ( r_ty ) ) ) ) )
              ( s_ty ) )
          [ ValD
              ( VarP ( 'supp ) )
              ( NormalB ( AppE
                  ( VarE 'evUr )
                  ( LamE
                      [ WildP
                      , VarP b_ex_nm ]
                      ( VarE b_ex_nm) ) ) )
              [ ]
          , SigD
              ( 'supp )
              ( ForallT
                  [ KindedTV
                      ( b_ty_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( s_ty ) ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''Ur )
                          ( VarT a_nm ) ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( VarT b_ty_nm ) )
                          ( VarT b_ty_nm ) ) ) )
          , PragmaD ( InlineP
              ( 'supp )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )

{- | Given arguments @a_ty@, @s@
    representing a type (of any representation)
    and a promoted term of type 'RuntimeRep',
    generates a 'Supp' instance
    via unsafe linearity coercion\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_SUPPABLE(a_ty, s)                         \
        instance Suppable (a_ty) (s) where                       \
            {-# INLINE CONLIKE supp #-}                          \
          ; supp :: forall (b :: TYPE s). a_ty %One-> b %One-> b \
          ; supp = case unsafeEqualityProof @Many @One of        \
                UnsafeRefl -> \ _ b -> b
    @
    Requires @-XDataKinds -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XScopedTypeVariables -XTypeApplications@\;
    if @repGrp s@ is not 'Prim' then requires @-XFlexibleInstances@.
    Throws @-Winaccessible-code@ and @-Woverlapping-patterns@.
    Potentially throws @-Woprhans@.
-}
deriveSuppable :: Type -> RuntimeRep -> Q Dec
deriveSuppable = \ a_ty s -> do
    guardExts
      ( "\'Prelude.Linear.deriveSuppable\'" )
      [ DataKinds
      , InstanceSigs
      , LinearTypes
      , MultiParamTypeClasses
      , PolyKinds
      , ScopedTypeVariables
      , TypeApplications ]
    case repGrp s of
        Prim -> pure ()
        _    -> guardExts
          ( "@Prelude.Linear.deriveSuppable _ (" <> show s <> ")@" )
          [ FlexibleInstances ]
    let s_ty = repType s
    guardNoInstance
      ( "@Prelude.Linear.deriveSuppable (" <> show a_ty <> ") (" <> show s <> ")@" )
      ( ''Suppable )
      [ a_ty
      , s_ty ]    
    b_ty_nm <- newName "b"
    b_ex_nm <- newName "b"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT ( AppT
              ( ConT ''Suppable )
              ( a_ty ) )
              ( s_ty ) )
          [ ValD
              ( VarP ( 'supp ) )
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
                      ( NormalB ( LamE
                          [ WildP
                          , VarP b_ex_nm ]
                          ( VarE b_ex_nm ) ) )
                      [ ] ] ) )
              [ ]
          , SigD
              ( 'supp )
              ( ForallT
                  [ KindedTV
                      ( b_ty_nm )
                      ( SpecifiedSpec )
                      ( AppT
                          ( ConT ''TYPE )
                          ( s_ty ) ) ]
                  [ ]
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( a_ty ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( VarT b_ty_nm ) )
                          ( VarT b_ty_nm ) ) ) )
          , PragmaD ( InlineP
              ( 'supp )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )