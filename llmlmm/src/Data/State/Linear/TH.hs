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
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | 'State#'-parametrized linear allocation tokens,
    representation-polymorphic interface to the suppression thereof,
    and TemplateHaskell generation of its instances
-}
module Data.State.Linear.TH
  ( -- * 'State#'-parametrized linear allocation tokens
    LAlloc#
      ( LAlloc# )
    -- * Representation-polymorphic linear allocation token suppression
  , Supp
      ( supp )
    -- * TemplateHaskell generation of linear allocation token suppression instances
  , deriveSupp
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
  , pattern Match
  , pattern CaseE
  , pattern VarE
  , pattern LamE
  , pattern AppTypeE
  , pattern PromotedT
  , pattern MulArrowT
  , pattern ConT
  , pattern AppT
  , pattern VarT
  , pattern KindedTV
  , pattern SpecifiedSpec
  , pattern ForallT
  , pattern WildP
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
  )

-- ++ (internal)

import Data.RuntimeRep
  ( repType )


-- * 'State#'-parametrized linear allocation tokens

{- | 'State#'-parametrized linear allocation tokens -}
type role LAlloc# nominal
newtype LAlloc# :: TYPE (BoxedRep Lifted) -> TYPE (TupleRep '[]) where
    LAlloc# ::
        forall (s :: TYPE (BoxedRep Lifted)).
        State# s %One-> LAlloc# s


-- * Representation-polymorphic linear allocation token suppression

{- | Representation-polymorphic linear allocation token suppression -}
class Supp (r :: RuntimeRep) where
    infixr 0 `supp`
    supp :: forall (a :: TYPE r) t. LAlloc# t %One-> a %One-> a


-- * TemplateHaskell generation of unboxed unit suppression instances

{- | Given argument @r@, representing a promoted term of type 'RuntimeRep',
    generates a 'Supp' instance for the latter via unsafe linearity coercion\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_SUPP(r)                                          \
        instance Supp (r) where                                         \
            {-# INLINE supp #-}                                         \
          ; supp :: forall (a :: TYPE r) t. LAlloc# t %One-> a %One-> a \
          ; supp = case unsafeEqualityProof @Many @One of               \
                UnsafeRefl -> \ _ a -> a
    @
    Requires at least @-XDataKinds -XInstanceSigs -XLinearTypes -XPolyKinds -XTemplateHaskell -XTypeApplications@,
    but this is not checked.
-}
deriveSupp :: RuntimeRep -> Dec
deriveSupp = \ r ->
    let r_ty = repType r
        a_ty_nm = mkName "a"
        a_ex_nm = mkName "a"
        t_nm = mkName "t"
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
                          ( r_ty ) )
                  , KindedTV
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
                      ( PromotedT 'One ) )
                      ( AppT
                          ( ConT ''LAlloc# )
                          ( VarT t_nm ) ) )
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