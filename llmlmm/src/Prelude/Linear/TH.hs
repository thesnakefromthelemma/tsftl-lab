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

  * Add and utilize TemplateHaskell support for constructor multiplicity (cf. GHC-65904)

  * Add TemplateHaskell support for quantified class instance declarations (cf. GHC-71492)
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
  , declareUrlike
  , declareUrlikeUnit
  , declareUrlikeUr
  , deriveUrlike
    -- * Representation-polymorphic interface to linearly suppressible types
  , Supp
      ( supp )
  , declareSuppViaUrlike
  , deriveSupp
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( pattern Lifted
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
  , pattern FlexibleContexts
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
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardValue
  , guardType
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
        TYPE r -> TYPE (BoxedRep Lifted)
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
        #define DERIVE_URABLE(r, cn_nm)                                  \
        instance Urable (r) where                                        \
            data instance Ur :: TYPE (r) -> TYPE (BoxedRep Lifted) where \
                cn_nm ::                                                 \
                    forall (a :: TYPE (r)).                              \
                    {-# UNPACK #-} !a %Many-> Ur a                       \
          ; {-# INLINE CONLIKE ur #-}                                    \
          ; ur ::                                                        \
                forall (a :: TYPE (r)).                                  \
                a %Many-> Ur a                                           \
          ; ur = cn_nm                                                   \
          ; {-# INLINE CONLIKE evUr #-}                                  \
          ; evUr ::                                                      \
                forall (a :: TYPE (r)) {s :: RuntimeRep} (b :: TYPE s).  \
                (a %Many-> b) %One-> Ur a %One-> b                       \
          ; evUr = \ f (cn_nm a) -> f a
    @
    Requires @-XDataKinds -XGADTSyntax -XInstanceSigs -XLinearTypes -XPolyKinds -XScopedTypeVariables -XTypeFamilies@\;
    if @repGrp r@ is not 'Prim' then requires @-XFlexibleInstances@\;
    if @r@ is not 'BoxedRep Lifted' then requires @-XMagicHash@.
-}
deriveUrable :: RuntimeRep -> Q Dec
deriveUrable = \ r -> do
    guardExts
      ( "\"Prelude.Linear.deriveUrable\"" )
      [ DataKinds
      , GADTSyntax
      , InstanceSigs
      , LinearTypes
      , PolyKinds
      , ScopedTypeVariables
      , TypeFamilies ]
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
                          ( PromotedT 'Lifted ) ) ) ) )
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

-- ** TemplateHaskell generation of representation-polymorphic interface to unrestricted-like types

{-  Morally equivalent to the @CPP@ macro
    @
        #define DECLARE_URLIKE                             \
        type Urlike ::                                     \
            forall {r :: RuntimeRep}. TYPE r -> Constraint \
        class Urlike a where                               \
            rep0 :: a %One-> (# #)                         \
          ; rep1 :: a %One-> (# a #)                       \
          ; rep2 :: a %One-> (# a, a #)                    \
            ...
          ; rep64 :: a %One-> (# a, ..., a #)
    @
    Requires @-XLinearTypes -XPolyKinds -XStandaloneKindSignatures -XUnboxedTuples@.
-}
declareUrlike :: Q [Dec]
declareUrlike = do
    guardExts
      ( "\"Prelude.Linear.declareUrlike\"" )
      [ LinearTypes
      , PolyKinds
      , StandaloneKindSignatures
      , UnboxedTuples ]
    let urlike_nm = mkName "Urlike"
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
            n <- [ 0 .. 64 ]
#else
            n <- [ 0 .. 8 ]
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
          ( urlike_nm )
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
          ( urlike_nm )
          [ KindedTV
              ( a_nm )
              ( BndrReq )
              ( AppT
                  ( ConT ''TYPE )
                  ( VarT r_nm ) ) ]
          [ ]
          ( srep_dc ) ]

-- ** TemplateHaskell generation of unrestricted-like instances

{- | Generates an 'Urlike' instance for @(# #)@\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLARE_URLIKE_UNIT                       \
        instance Urlike (# #) where                       \
            {-# INLINE CONLIKE rep0 #-}                   \
          ; rep0 :: (# #) %One-> (# #)                    \
          ; rep0 = \ (# #) -> (# #)                       \
          ; {-# INLINE CONLIKE rep1 #-}                   \
          ; rep1 :: (# #) %One-> (# (# #) #)              \
          ; rep1 = \ (# #) -> (# (# #) #)                 \
          ; {-# INLINE CONLIKE rep2 #-}                   \
          ; rep2 :: (# #) %One-> (# (# #), (# #) #)       \
          ; rep2 = \ (# #) -> (# (# #), (# #) #)          \
            ...
          ; {-# INLINE CONLIKE rep64 #-}                  \
          ; rep64 :: (# #) %One-> (# (# #), ..., (# #) #) \
          ; rep64 = \ (# #) -> (# (# #), ..., (# #) #)
    @
    Requires @-XDataKinds -XInstanceSigs -XLinearTypes -XUnboxedTuples@.
    Requires that @'Prelude.Linear.Urlike' (..)@ be in scope.
-}
declareUrlikeUnit :: Q Dec
declareUrlikeUnit = do
    guardExts
      ( "\"Prelude.Linear.declareUrlikeUnit\"" )
      [ DataKinds
      , InstanceSigs
      , LinearTypes
      , UnboxedTuples ]
    urlike_nm <- guardType
      ( "\"Prelude.Linear.declareUrlikeUnit\"" )
      ( "Prelude.Linear.Urlike" )
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
            n <- [ 0 .. 64 ]
#else
            n <- [ 0 .. 8 ]
#endif
            pure $ do
                rep_n_nm <- guardValue
                  ( "\"Prelude.Linear.declareUrlikeUnit\"" )
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
              ( ConT urlike_nm )
              ( UnboxedTupleT 0 ) )
          ( srep_dec ) )

{- | Given argument @r@, generates an 'Urlike' instance
    for all @Ur a@ with @a@ a type of representation @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLEAR_URLIKE_UR(r)                       \
        instance forall (a :: TYPE r). Urlike (Ur a) where \
            {-# INLINE CONLIKE rep0 #-}                    \
          ; rep0 :: Ur a %One-> (# #)                      \
          ; rep0 = evUr (\ _ -> (# #))                     \
          ; {-# INLINE CONLIKE rep1 #-}                    \
          ; rep1 :: Ur a %One-> (# Ur a #)                 \
          ; rep1 = \ ua -> (# ua #)                        \
          ; {-# INLINE CONLIKE rep2 #-}                    \
          ; rep2 :: Ur a %One-> (# Ur a, Ur a #)           \
          ; rep2 = evUr (\ a -> (# ur a, ur a #))          \
            ...
          ; {-# INLINE CONLIKE rep64 #-}                   \
          ; rep64 :: Ur a %One-> (# Ur a, ..., Ur a #)     \
          ; rep64 = evUr (\ a -> (# ur a, ..., ur a #))
    @
    Requires @-XDataKinds -XFlexibleInstances -XInstanceSigs -XLinearTypes -XPolyKinds -XUnboxedTuples@.
    Requires that @'Prelude.Linear.Urlike' (..)@ be in scope.
-}
declareUrlikeUr :: RuntimeRep -> Q Dec
declareUrlikeUr = \ r -> do
    guardExts
      ( "\"Prelude.Linear.declareUrlikeUr\"" )
      [ DataKinds
      , FlexibleInstances
      , InstanceSigs
      , LinearTypes
      , PolyKinds
      , UnboxedTuples ]
    urlike_nm <- guardType
      ( "\"Prelude.Linear.declareUrlikeUr\"" )
      ( "Prelude.Linear.Urlike" )
    let r_ty = repType r
    a_ex_nm <- newName "a"
    a_ty_nm <- newName "a"
    let rep_n_nm_ug = \ n -> "Prelude.Linear.rep" <> show n
    let stup_n_ex = \ n -> do
            (_ :: Int) <- [ 0 .. n - 1 ]
            [ Just ( AppE
                ( VarE 'ur )
                ( VarE a_ex_nm ) ) ]
    let rep_n_ex = \case
            0 ->
              ( AppE
                  ( VarE 'evUr )
                  ( LamE
                      [ WildP ]
                      ( UnboxedTupE [ ] ) ) )
            1 ->
              ( UnboxedTupE [ Nothing ] )
            n ->
              ( AppE
                  ( VarE 'evUr )
                  ( LamE
                      [ VarP a_ex_nm ]
                      ( UnboxedTupE ( stup_n_ex n ) ) ) )
    let tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( AppT
                  ( ConT ''Ur )
                  ( VarT a_ty_nm ) )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    srep_dec <- fmap join . sequence $ do
#if FULL
            n <- [ 0 .. 64 ]
#else
            n <- [ 0 .. 8 ]
#endif
            pure $ do
                rep_n_nm <- guardValue
                  ( "\"Prelude.Linear.declareUrlikeUr\"" )
                  ( rep_n_nm_ug n )
                pure
                  [ ValD
                      ( VarP ( rep_n_nm ) )
                      ( NormalB ( rep_n_ex n ) )
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
                  ( ConT urlike_nm )
                  ( AppT
                      ( ConT ''Ur )
                      ( VarT a_ty_nm ) ) ) )-} -- GHC-71492 :(
          ( AppT
              ( ConT urlike_nm )
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
    generates an 'Urlike' instance for the latter
    via unsafe linearity coercion\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_URLIKE(a_ty)                        \
        instance Urlike (a_ty) where                       \
            {-# INLINE CONLIKE rep0 #-}                    \
          ; rep0 :: a %One-> (# #)                         \
          ; rep0 = case unsafeEqualityProof @Many @One of  \
                UnsafeRefl -> \ _ -> (# #)                 \
          ; {-# INLINE CONLIKE rep1 #-}                    \
          ; rep1 :: a %One-> (# a #)                       \
          ; rep1 = \ a -> (# a #)                          \
          ; {-# INLINE CONLIKE rep2 #-}                    \
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
    Requires that @'Prelude.Linear.Urlike' ( .. )@ be in scope.
-}
deriveUrlike :: Type -> Q Dec
deriveUrlike = \ a_ty -> do
    guardExts
      ( "\"Prelude.Linear.deriveUrlike\"" )
      [ DataKinds
      , InstanceSigs
      , LinearTypes
      , TupleSections
      , TypeApplications
      , UnboxedTuples ]
    urlike_nm <- guardType
      ( "\"Prelude.Linear.deriveUrlike\"" )
      ( "Prelude.Linear.Urlike" )
    a_ex_nm <- newName "a"
    let rep_n_nm_ug = \ n -> "Prelude.Linear.rep" <> show n
    let stup_n_ex = \ n -> do
            (_ :: Int) <- [ 0 .. n - 1 ]
            [ Just ( VarE a_ex_nm ) ]
    let rep_n_ex = \case
            0 ->
              ( CaseE
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
                          [ WildP ]
                          ( UnboxedTupE [ ] ) ) )
                      [ ] ] )
            1 ->
              ( UnboxedTupE [ Nothing ] )
            n ->
             ( CaseE
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
                      [ ] ] )
    let tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( a_ty )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
    srep_dec <- fmap join . sequence $ do
#if FULL
            n <- [ 0 .. 64 ]
#else
            n <- [ 0 .. 8 ]
#endif
            pure $ do
                rep_n_nm <- guardValue
                  ( "\"Prelude.Linear.deriveUrlike\"" )
                  ( rep_n_nm_ug n )
                pure
                  [ ValD
                      ( VarP ( rep_n_nm ) )
                      ( NormalB ( rep_n_ex n ) )
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
              ( ConT urlike_nm )
              ( a_ty ) )
          ( srep_dec ) )


-- * Representation-polymorphic interface to linearly suppressible types

-- ** Representation-polymorphic interface to linearly suppressible types

type Supp ::
    forall {r :: RuntimeRep}. TYPE r -> RuntimeRep -> Constraint
class Supp a (s :: RuntimeRep) where
    infixr 0 `supp`
    supp :: forall (b :: TYPE s). a %One-> b %One-> b

-- ** TemplateHaskell generation of linearly suppressible instances

{- | Given argument @s@
    representing a promoted term of type 'RuntimeRep',
    generates a 'Supp' instance
    contingent on an 'Urlike' instance\;
    morally equivalent to the @CPP@ macro
    @
        #define DECLARE_SUPP_VIA_URLIKE(s)                    \
        instance Urlike a => Supp a (s) where                 \
            {-# INLINE CONLIKE supp #-}                       \
          ; supp :: forall (b :: TYPE s). a %One-> b %One-> b \
          ; supp = \ a -> case rep0 a of (# #) -> \ b -> b
    @
    Requires @-XDataKinds -XFlexibleContexts -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XScopedTypeVariables -XUnboxedTuples@\;
    if @repGrp s@ is not 'Prim' then requires @-XFlexibleInstances@.
    Requires that @'Prelude.Linear.Urlike' ( 'Prelude.Linear.rep0' )@ be in scope.
-}
declareSuppViaUrlike :: RuntimeRep -> Q Dec
declareSuppViaUrlike = \ s -> do
    guardExts
      ( "\"Prelude.Linear.declareSuppViaUrlike\"" )
      [ DataKinds
      , FlexibleContexts
      , InstanceSigs
      , LinearTypes
      , MultiParamTypeClasses
      , PolyKinds
      , ScopedTypeVariables
      , UnboxedTuples ]
    case repGrp s of
        Prim -> pure ()
        _    -> guardExts
          ( "@Prelude.Linear.declareSuppViaUrlike (" <> show s <> ")" )
          [ FlexibleInstances ]
    urlike_nm <- guardType
      ( "\"Prelude.Linear.declareSuppViaUrlike\"" )
      ( "Prelude.Linear.Urlike" )
    a_ty_nm <- newName "a"
    let s_ty = repType s
    b_ty_nm <- newName "b"
    rep0_nm <- guardValue
      ( "\"Prelude.Linear.declareSuppViaUrlike\"" )
      ( "Prelude.Linear.rep0" )
    a_ex_nm <- newName "a"
    b_ex_nm <- newName "b"
    pure
      ( InstanceD
          ( Nothing )
          [ AppT
              ( ConT urlike_nm )
              ( VarT a_ty_nm ) ]
          ( AppT ( AppT
              ( ConT ''Supp )
              ( VarT a_ty_nm ) )
              ( s_ty ) )
          [ ValD
              ( VarP 'supp )
              ( NormalB ( LamE
                  [ VarP a_ex_nm ]
                  ( CaseE
                      ( AppE
                          ( VarE rep0_nm )
                          ( VarE a_ex_nm ) )
                      [ Match
                          ( UnboxedTupP [ ] )
                          ( NormalB ( LamE
                              [ VarP b_ex_nm ]
                              ( VarE b_ex_nm ) ) )
                          [ ] ] ) ) )
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
                      ( VarT a_ty_nm ) )
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
        #define DERIVE_SUPP(a_ty, s)                             \
        instance Supp (a_ty) (s) where                           \
            {-# INLINE CONLIKE supp #-}                          \
          ; supp :: forall (b :: TYPE s). a_ty %One-> b %One-> b \
          ; supp = case unsafeEqualityProof @Many @One of        \
                UnsafeRefl -> \ _ b -> b
    @
    Requires @-XDataKinds -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XScopedTypeVariables -XTypeApplications@\;
    if @repGrp s@ is not 'Prim' then requires @-XFlexibleInstances@.
    Requires that @'Prelude.Linear.Supp' ( 'Prelude.Linear.supp' )@ be in scope.
-}
deriveSupp:: Type -> RuntimeRep -> Q Dec
deriveSupp = \ a_ty s -> do
    guardExts
      ( "\"Prelude.Linear.deriveSupp\"" )
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
          ( "@Prelude.Linear.deriveUrable _ (" <> show s <> ")@" )
          [ FlexibleInstances ]
    let s_ty = repType s
    supp_cl_nm <- guardType
      ( "\"Prelude.Linear.deriveSupp\"" )
      ( "Prelude.Linear.Supp" )
    supp_ex_nm <- guardValue
      ( "\"Prelude.Linear.deriveSupp\"" )
      ( "Prelude.Linear.supp" )
    b_ty_nm <- newName "b"
    b_ex_nm <- newName "b"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT ( AppT
              ( ConT supp_cl_nm )
              ( a_ty ) )
              ( s_ty ) )
          [ ValD
              ( VarP ( supp_ex_nm ) )
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
              ( supp_ex_nm )
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
              ( supp_ex_nm )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )