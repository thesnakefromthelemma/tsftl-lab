{-# LANGUAGE Haskell2010
  , DataKinds
  , LambdaCase
  , LinearTypes
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , TemplateHaskellQuotes
  , TypeFamilies
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | TemplateHaskell generation of unrestricted-related utilities/interfaces -}
module Prelude.Linear.TH
  ( -- * Representation-polymorphic interface to strict unrestricted modality
    -- ** Representation-polymorphic interface to strict unrestricted modality
    Urable
      ( Ur
      , ur
      , evUr
      )
    -- ** TemplateHaskell generation of strict unrestricted modalities
  , deriveUrable
    -- * Representation-polymorphic unrestricted-like types
    -- ** TemplateHaskell generation of representation-polymorphic interface to unrestricted-like types
  , declareUrlike
    -- ** TemplateHaskell generation of unrestricted-like instances
  , deriveUrlike
  , declareUrlikeUr
    -- * Representation-polymorphic unboxed unit suppression
    -- ** Representation-polymorphic unboxed unit suppression
  , Supp
       ( supp )
    -- ** TemplateHaskell generation of unboxed unit suppression instances
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
  )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
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
  , pattern BndrReq
  , pattern ForallT
  , pattern SigT
  , pattern WildP
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern NormalB
  , pattern ValD
  , pattern SigD
  , pattern NoSourceUnpackedness
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
  )

-- ++ (internal)

import Data.RuntimeRep
  ( repType
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
        forall (a :: TYPE r) (s :: RuntimeRep) (b :: TYPE s).
        (a %Many-> b) %1 -> Ur a %1 -> b

-- ** TemplateHaskell generation of strict unrestricted modalities

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates a strict unrestricted modality ('Ur') instance
    for types of the representation corresponding to @r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_URABLE(r_ty, cn_nm)                                  \
        instance Urable (r_ty) where                                        \
            data instance Ur :: TYPE (r_ty) -> TYPE (BoxedRep Lifted) where \
                cn_nm ::                                                    \
                    forall (a :: TYPE (r_ty)).                              \
                    !a %Many-> Ur a                                         \
          ; {-# INLINE ur #-}                                               \
          ; ur ::                                                           \
                forall (a :: TYPE (r_ty)).                                  \
                a %Many-> Ur a                                              \
          ; ur = cn_nm                                                      \
          ; {-# INLINE evUr #-}                                             \
          ; evUr ::                                                         \
                forall (a :: TYPE (r_ty)) (s :: RuntimeRep) (b :: TYPE s).  \
                (a %Many-> b) %1 -> Ur a %1 -> b                            \
          ; evUr = \ f (cn_nm a) -> f a
    @
    Requires at least @-XDataKinds -XFlexibleInstances -XGADTSyntax -XInstanceSigs -XLinearTypes -XPolyKinds -XScopedTypeVariables -XTemplateHaskell -XTypeFamilies@
    (but this is not checked).
-}
deriveUrable :: RuntimeRep -> Dec
deriveUrable = \ r ->
    let r_ty = repType r
        cn_nm = mkName $ "Ur" <> repStem r <> case r of BoxedRep Lifted -> ""; _ -> "#"
        a_ty_nm = mkName "a"
        f_nm = mkName "f"
        a_ex_nm = mkName "a"
        s_nm = mkName "s"
        b_nm = mkName "b"
    in  InstanceD
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
                      [ ( Bang NoSourceUnpackedness SourceStrict -- WARNING: We haven't specified a linerity\; cf. GHC-65904
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
                      ( SpecifiedSpec )
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
              ( AllPhases ) ) ]


-- * Representation-polymorphic unrestricted-like types

-- ** TemplateHaskell generation of representation-polymorphic interface to unrestricted-like types

{-  Morally equivalent to the @CPP@ macro
    @
        #define DECLARE_URLIKE                             \
        class Urlike (r :: RuntimeRep) (a :: TYPE r) where \
            rep0 :: a %One-> (# #)                         \
          ; rep1 :: a %One-> (# a #)                       \
          ; rep2 :: a %One-> (# a, a #)                    \
            ...
          ; rep64 :: a %One-> (# a, ..., a #)
    @
    Requires @-XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XTemplateHaskell -XUnboxedTuples@
    (but this is not checked).
-}
declareUrlike :: Dec
declareUrlike =
    let urlike_nm = mkName "Urlike"
        r_nm = mkName "r"
        a_nm = mkName "a"
        rep_n_nm = \ n -> mkName $ "rep" <> show n
        tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( VarT a_nm )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
        srep_dc = do
            n <- [ 0 .. 64 ] 
            [ SigD
              ( rep_n_nm n )
              ( AppT ( AppT ( AppT
                  ( MulArrowT )
                  ( PromotedT 'One ) )
                  ( VarT a_nm ) )
                  ( tup_n_ty n ) ) ]
    in  ClassD
          [ ]
          ( urlike_nm )
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
          ( srep_dc )

-- ** TemplateHaskell generation of unrestricted-like instances

{- | Given arguments @r@, @a_ty@,
    representing a promoted term of type 'RuntimeRep'
    and a type of that representation respectively,
    generates an 'Urlike' instance for the latter via unsafe linearity coercion\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_URLIKE(r, a_ty)                     \
        instance Urlike (r) (a_ty) where                   \
            {-# INLINE rep0 #-}                            \
          ; rep0 :: a %One-> (# #)                         \
          ; rep0 = case unsafeEqualityProof @Many @One of  \
                UnsafeRefl -> \ _ -> (# #)                 \
          ; {-# INLINE rep1 #-}                            \
          ; rep1 :: a %One-> (# a #)                       \
          ; rep1 = \ a -> (# a #)                          \
          ; {-# INLINE rep2 #-}                            \
          ; rep2 :: a %One-> (# a, a #)                    \
          ; rep2 = case unsafeEqualityProof @Many @One of  \
                UnsafeRefl -> \ a -> (# a, a #)            \
            ...
          ; {-# INLINE rep64 #-}                           \
          ; rep64 :: a %One-> (# a, ..., a #)              \
          ; rep64 = case unsafeEqualityProof @Many @One of \
                UnsafeRefl -> \ a -> (# a, ..., a #)
    @
    Requires at least @-XDataKinds -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XTemplateHaskell -XTupleSections -XTypeApplications -XUnboxedTuples@,
    but this is not checked.
-}
deriveUrlike :: RuntimeRep -> Type -> Dec
deriveUrlike = \ r a_ty ->
    let urlike_nm = mkName "Urlike"
        r_ty = repType r
        a_ex_nm = mkName "a"
        stup_n_ex = \ n -> do
            (_ :: Int) <- [ 0 .. n - 1 ]
            [ Just ( VarE a_ex_nm ) ]
        rep_n_ex = \case
            0 ->
                CaseE
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
                      [ ] ]
            1 ->
                UnboxedTupE [ Nothing ]
            n ->
                CaseE
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
                      [ ] ]
        rep_n_nm = \ n -> mkName $ "rep" <> show n
        tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( a_ty )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
        srep_dec = do
            n <- [ 0 .. 64 ]
            id -- just for parser reasons...
              [ ValD
                  ( VarP ( rep_n_nm n ) )
                  ( NormalB ( rep_n_ex n ) )
                  [ ]
              , SigD
                  ( rep_n_nm n )
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( a_ty ) )
                      ( tup_n_ty n ) )
              , PragmaD ( InlineP
                  ( rep_n_nm n )
                  ( Inline )
                  ( ConLike )
                  ( AllPhases ) ) ]
    in  InstanceD
          ( Nothing )
          [ ]
          ( AppT ( AppT
              ( ConT urlike_nm )
              ( r_ty ) )
              ( a_ty ) )
          ( srep_dec )

{- | Given argument @r@, generates an 'Urlike' instance
    for all @Ur a@ with @a@ a type of representation @r@\;
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
    Requires at least  @-XDataKinds -XFlexibleInstances -XInstanceSigs -XLinearTypes -XMultiParamTypeClasses -XPolyKinds -XTemplateHaskell -XUnboxedTuples@,
    but this is not checked.
-}
declareUrlikeUr :: RuntimeRep -> Dec
declareUrlikeUr = \ r ->
    let urlike_nm = mkName "Urlike"
        r_ty = repType r
        a_ex_nm = mkName "a"
        a_ty_nm = mkName "a"
        stup_n_ex = \ n -> do
            (_ :: Int) <- [ 0 .. n - 1 ]
            [ Just ( AppE
                ( VarE 'ur )
                ( VarE a_ex_nm ) ) ]
        rep_n_ex = \case
            0 ->
                AppE
                  ( VarE 'evUr )
                  ( LamE
                      [ WildP ]
                      ( UnboxedTupE [ ] ) )
            1 ->
                UnboxedTupE [ Nothing ]
            n ->
                AppE
                  ( VarE 'evUr )
                  ( LamE
                      [ VarP a_ex_nm ]
                      ( UnboxedTupE ( stup_n_ex n ) ) )
        rep_n_nm = \ n -> mkName $ "rep" <> show n
        tup_n_ty = \ n -> foldr (\ _ b ->
            AppT
              ( b )
              ( AppT
                  ( ConT ''Ur )
                  ( VarT a_ty_nm ) )
          ) ( UnboxedTupleT n ) [ 0 .. n - 1 ]
        srep_dec = do
            n <- [ 0 .. 64 ] 
            id -- just for parser reasons...
              [ ValD
                  ( VarP ( rep_n_nm n ) )
                  ( NormalB ( rep_n_ex n ) )
                  [ ]
              , SigD
                  ( rep_n_nm n )
                  ( AppT ( AppT ( AppT
                      ( MulArrowT )
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''Ur )
                          ( VarT a_ty_nm ) ) )
                      ( tup_n_ty n ) )
              , PragmaD ( InlineP
                  ( rep_n_nm n )
                  ( Inline )
                  ( ConLike )
                  ( AllPhases ) ) ]
    in  InstanceD
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
              ( AppT ( AppT
                  ( ConT urlike_nm )
                  ( AppT
                      ( PromotedT 'BoxedRep )
                      ( PromotedT 'Lifted ) ) )
                  ( AppT
                      ( ConT ''Ur )
                      ( VarT a_ty_nm ) ) ) )-} -- GHC-71492 :(
          ( AppT ( AppT
              ( ConT urlike_nm )
              ( AppT
                  ( PromotedT 'BoxedRep )
                  ( PromotedT 'Lifted ) ) )
              ( AppT
                  ( ConT ''Ur )
                  ( SigT
                      ( VarT a_ty_nm )
                      ( AppT
                          ( ConT ''TYPE )
                          ( r_ty ) ) ) ) )
          ( srep_dec )


-- * Representation-polymorphic unboxed unit suppression

-- ** Representation-polymorphic unboxed unit suppression

{- | Representation-polymorphic unboxed unit suppression -}
class Supp (r :: RuntimeRep) where
    infixr 0 `supp`
    supp :: forall (a :: TYPE r). (# #) %One-> a %One-> a

-- ** TemplateHaskell generation of unboxed unit suppression instances

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates a 'Supp' instance for the latter via unsafe linearity coercion\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_SUPP(r)                                    \
        instance Supp (r) where                                   \
            {-# INLINE supp #-}                                   \
          ; supp :: forall (a :: TYPE r). (# #) %One-> a %One-> a \
          ; supp = case unsafeEqualityProof @Many @One of         \
                UnsafeRefl -> \ _ a -> a
    @
    Requires at least @-XDataKinds -XInstanceSigs -XLinearTypes -XPolyKinds -XTemplateHaskell -XTypeApplications -XUnboxedTuples@,
    but this is not checked.
-}
deriveSupp :: RuntimeRep -> Dec
deriveSupp = \ r ->
    let r_ty = repType r
        a_ty_nm = mkName "a"
        a_ex_nm = mkName "a"
    in  InstanceD
          ( Nothing )
          [ ]
          ( AppT
              ( ConT ''Supp )
              ( r_ty ) )
          [ ValD
              ( VarP 'supp )
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
                          , VarP a_ex_nm ]
                          ( VarE a_ex_nm ) ) )
                      [ ] ] ) )
              [ ]
          , SigD
              ( 'supp )
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
                      ( PromotedT 'One ) )
                      ( UnboxedTupleT 0 ) )
                      ( AppT ( AppT ( AppT
                          ( MulArrowT )
                          ( PromotedT 'One ) )
                          ( VarT a_ty_nm ) )
                          ( VarT a_ty_nm ) ) ) )
          , PragmaD ( InlineP
              ( 'supp )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ]