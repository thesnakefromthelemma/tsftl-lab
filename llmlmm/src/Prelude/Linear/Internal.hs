{-# LANGUAGE Haskell2010
  , DataKinds
  , LinearTypes
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Strict unrestricted modalities for each representation type -}
module Prelude.Linear.Internal
  ( -- * Representation-polymorphic interface to strict unrestricted modality
    Urable
      ( Ur
      , ur
      , evUr
      )
    -- * TemplateHaskell generation of strict unrestricted modalities
  , deriveUrable
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , RuntimeRep
      ( BoxedRep )
  , pattern Lifted
  , pattern One
  , pattern Many
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( Name
  , mkName
  , pattern ConE
  , pattern VarE
  , pattern LamE
  , pattern AppE
  , pattern NormalB
  , Type
  , pattern PromotedT
  , pattern ConT
  , pattern AppT
  , pattern ArrowT
  , pattern MulArrowT
  , pattern VarT
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , pattern ConP
  , pattern VarP
  , Dec
  , pattern ValD
  , pattern SigD
  , pattern NoSourceUnpackedness
  , pattern SourceStrict
  , pattern Bang
  , pattern GadtC
  , pattern ForallC
  , pattern DataInstD
  , pattern InstanceD
  , pattern Inline
  , pattern ConLike
  , pattern AllPhases
  , pattern InlineP
  , pattern PragmaD
  )


-- * Interface to representation-polymorphic strict unrestricted modality

{- | Interface to representation-polymorphic strict unrestricted modality -}
class Urable (r :: RuntimeRep) where
    data Ur ::
        TYPE r -> TYPE (BoxedRep Lifted)
    ur ::
        forall (a :: TYPE r).
        a %Many -> Ur a
    evUr ::
        forall (a :: TYPE r) (s :: RuntimeRep) (b :: TYPE s).
        (a %Many -> b) %1 -> Ur a %1 -> b


-- * TemplateHaskell generation of strict unrestricted modalities

{- | Given arguments @r_ty@, @cn_nm@,
    the first representing a promoted term of type 'RuntimeRep',
    generates a strict unrestricted modality ('Ur') data instance with constructor named @cn_nm@
    for types of the representation corresponding to @r_ty@.
    Morally equivalent to the @CPP@ macro
    @
        #define DERIVE_URABLE(r_ty, cn_nm)                                  \
        instance Urable (r_ty) where                                        \
            data instance Ur :: TYPE (r_ty) -> TYPE (BoxedRep Lifted) where \
                cn_nm ::                                                    \
                    forall (a :: TYPE (r_ty)).                              \
                    !a %Many -> Ur a                                        \
          ; {-# INLINE CONLIKE ur #-}                                       \
          ; ur ::                                                           \
                forall (a :: TYPE (r_ty)).                                  \
                a %Many -> Ur a                                             \
          ; ur = cn_nm                                                      \
          ; {-# INLINE CONLIKE evUr #-}                                     \
          ; evUr ::                                                         \
                forall (a :: TYPE (r_ty)) (s :: RuntimeRep) (b :: TYPE s).  \
                (a %Many -> b) %1 -> Ur a %1 -> b                           \
          ; evUr = \ f (cn_nm a) -> f a
    @
    Requires at least @-XDataKinds -XFlexibleInstances -XGADTSyntax -XInstanceSigs -XLinearTypes -XPolyKinds -XScopedTypeVariables -XTemplateHaskell -XTypeFamilies@.
-}
deriveUrable :: Type -> Name -> Dec
deriveUrable = \ r_ty cn_nm ->
    let a_ty_nm = mkName "a"
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