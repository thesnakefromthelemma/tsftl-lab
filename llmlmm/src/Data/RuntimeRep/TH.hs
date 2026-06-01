{-# LANGUAGE Haskell2010
  , AllowAmbiguousTypes
  , CPP
  , GADTSyntax
  , LambdaCase
  , PatternSynonyms
  , PolyKinds
  , RequiredTypeArguments
  , ScopedTypeVariables
  , TemplateHaskellQuotes
#-}

{-| @-Wunused-foralls@ is disabled so that we can
    define method 'ofType' of class 'Rep'.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-unused-foralls
#-}

{- | TemplateHaskell generation of representation type-to-term demotion -}
module Data.RuntimeRep.TH
  ( -- * Fundamental representation groups
    RepGrp
      ( Prim
      , Lim
      , Vec
      , Box
      )
  , repGrp
    -- * TemplateHaskell promotion
  , elemExp
  , countExp
  , levityExp
  , repExp
  , elemType
  , countType
  , levityType
  , repType
    -- * Type-to-term demotion
  , Rep
      ( repTerm
      , repOf
      )
  , declareRep
  ) where


-- + Imports

import Prelude hiding
  ( elem )

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( VecElem
      ( Int8ElemRep
      , Int16ElemRep
      , Int32ElemRep
      , Int64ElemRep
      , Word8ElemRep
      , Word16ElemRep
      , Word32ElemRep
      , Word64ElemRep
      , FloatElemRep
      , DoubleElemRep
      )
  , VecCount
      ( Vec2
      , Vec4
      , Vec8
      , Vec16
      , Vec32
      , Vec64
      )
  , Levity
      ( Unlifted
      , Lifted
      )
  , RuntimeRep
      ( Int8Rep
      , Int16Rep
      , Int32Rep
      , Int64Rep
      , IntRep
      , Word8Rep
      , Word8Rep
      , Word16Rep
      , Word32Rep
      , Word64Rep
      , WordRep
      , AddrRep
      , FloatRep
      , DoubleRep
      , TupleRep
      , SumRep
      , VecRep
      , BoxedRep )
  , TYPE
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( newName
  , Exp
  , pattern ListE
  , pattern ConE
  , pattern AppE
  , pattern LamE
  , Type
  , pattern PromotedT
  , pattern PromotedNilT
  , pattern PromotedConsT
  , pattern ConT
  , pattern AppT
  , pattern KindedTV
  , pattern ForallVisT
  , pattern WildP
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
  , Q
  , pattern AllowAmbiguousTypes
  , pattern DataKinds
  , pattern FlexibleInstances
  , pattern InstanceSigs
  , pattern PolyKinds
  , pattern RequiredTypeArguments
  , pattern ScopedTypeVariables
  )

-- ++ (internal)

import Misc.TH
  ( guardExts
  , guardNoInstance
  )


-- * Fundamental representation groups

{- | Broad categories of 'RuntimeRep's,
    grouped by outermost constructor type
-}
data RepGrp where
    Prim, Lim, Vec, Box :: RepGrp

{- | Given argument @r@,
    returns the 'RepGrp' corresponding to @r@
-}
repGrp :: RuntimeRep -> RepGrp
repGrp = \case
    Int8Rep    -> Prim
    Int16Rep   -> Prim
    Int32Rep   -> Prim
    Int64Rep   -> Prim
    IntRep     -> Prim
    Word8Rep   -> Prim
    Word16Rep  -> Prim
    Word32Rep  -> Prim
    Word64Rep  -> Prim
    WordRep    -> Prim
    AddrRep    -> Prim
    FloatRep   -> Prim
    DoubleRep  -> Prim
    TupleRep _ -> Lim
    SumRep _   -> Lim
    VecRep _ _ -> Vec
    BoxedRep _ -> Box


-- * TemplateHaskell promotion

-- ** TemplateHaskell promotion to 'Exp'

{- | Given argument @e@,
    returns @e@ as a TemplateHaskell expression
-}
elemExp :: VecElem -> Exp
elemExp = \case
    Int8ElemRep   -> ConE 'Int8ElemRep
    Int16ElemRep  -> ConE 'Int16ElemRep
    Int32ElemRep  -> ConE 'Int32ElemRep
    Int64ElemRep  -> ConE 'Int64ElemRep
    Word8ElemRep  -> ConE 'Word8ElemRep
    Word16ElemRep -> ConE 'Word16ElemRep
    Word32ElemRep -> ConE 'Word32ElemRep
    Word64ElemRep -> ConE 'Word64ElemRep
    FloatElemRep  -> ConE 'FloatElemRep
    DoubleElemRep -> ConE 'DoubleElemRep

{- | Given argument @c@,
    returns @c@ as a TemplateHaskell expression
-}
countExp :: VecCount -> Exp
countExp = \case
    Vec2  -> ConE 'Vec2
    Vec4  -> ConE 'Vec4
    Vec8  -> ConE 'Vec8
    Vec16 -> ConE 'Vec16
    Vec32 -> ConE 'Vec32
    Vec64 -> ConE 'Vec64

{- | Given argument @l@,
    returns @l@ as a TemplateHaskell expression
-}
levityExp :: Levity -> Exp
levityExp = \case
    Unlifted -> ConE 'Unlifted
    Lifted   -> ConE 'Lifted

{- | Given argument @r@,
    returns @r@ as a TemplateHaskell expression
-}
repExp :: RuntimeRep -> Exp
repExp = \case
    Int8Rep           -> ConE 'Int8Rep
    Int16Rep          -> ConE 'Int16Rep
    Int32Rep          -> ConE 'Int32Rep
    Int64Rep          -> ConE 'Int64Rep
    IntRep            -> ConE 'IntRep
    Word8Rep          -> ConE 'Word8Rep
    Word16Rep         -> ConE 'Word16Rep
    Word32Rep         -> ConE 'Word32Rep
    Word64Rep         -> ConE 'Word64Rep
    WordRep           -> ConE 'WordRep
    AddrRep           -> ConE 'AddrRep
    FloatRep          -> ConE 'FloatRep
    DoubleRep         -> ConE 'DoubleRep
    TupleRep sr       ->
        let sr_ex = map repExp sr
        in  AppE
              ( ConE 'TupleRep )
              ( ListE sr_ex )
    SumRep sr         ->
        let sr_ex = map repExp sr
        in  AppE
              ( ConE 'SumRep )
              ( ListE sr_ex )
    VecRep count elem ->
      ( AppE ( AppE
          ( ConE 'VecRep )
          ( countExp count ) )
          ( elemExp elem ) )
    BoxedRep levity   ->
      ( AppE
          ( ConE 'BoxedRep )
          ( levityExp levity ) )

-- ** TemplateHaskell promotion to 'Type'

{- | Given argument @e@,
    returns the promoted type of @e@ as a TemplateHaskell type
-}
elemType :: VecElem -> Type
elemType = \case
    Int8ElemRep   -> PromotedT 'Int8ElemRep
    Int16ElemRep  -> PromotedT 'Int16ElemRep
    Int32ElemRep  -> PromotedT 'Int32ElemRep
    Int64ElemRep  -> PromotedT 'Int64ElemRep
    Word8ElemRep  -> PromotedT 'Word8ElemRep
    Word16ElemRep -> PromotedT 'Word16ElemRep
    Word32ElemRep -> PromotedT 'Word32ElemRep
    Word64ElemRep -> PromotedT 'Word64ElemRep
    FloatElemRep  -> PromotedT 'FloatElemRep
    DoubleElemRep -> PromotedT 'DoubleElemRep

{- | Given argument @c@,
    returns the promoted type of @c@ as a TemplateHaskell type
-}
countType :: VecCount -> Type
countType = \case
    Vec2  -> PromotedT 'Vec2
    Vec4  -> PromotedT 'Vec4
    Vec8  -> PromotedT 'Vec8
    Vec16 -> PromotedT 'Vec16
    Vec32 -> PromotedT 'Vec32
    Vec64 -> PromotedT 'Vec64

{- | Given argument @l@,
    returns the promoted type of @l@ as a TemplateHaskell type
-}
levityType :: Levity -> Type
levityType = \case
    Unlifted -> PromotedT 'Unlifted
    Lifted   -> PromotedT 'Lifted

{- | Given argument @r@,
    returns the promoted type of @r@ as a TemplateHaskell type
-}
repType :: RuntimeRep -> Type
repType = \case
    Int8Rep           -> PromotedT 'Int8Rep
    Int16Rep          -> PromotedT 'Int16Rep
    Int32Rep          -> PromotedT 'Int32Rep
    Int64Rep          -> PromotedT 'Int64Rep
    IntRep            -> PromotedT 'IntRep
    Word8Rep          -> PromotedT 'Word8Rep
    Word16Rep         -> PromotedT 'Word16Rep
    Word32Rep         -> PromotedT 'Word32Rep
    Word64Rep         -> PromotedT 'Word64Rep
    WordRep           -> PromotedT 'WordRep
    AddrRep           -> PromotedT 'AddrRep
    FloatRep          -> PromotedT 'FloatRep
    DoubleRep         -> PromotedT 'DoubleRep
    TupleRep sr       ->
        let sr_ty = foldr (\ r b ->
                AppT ( AppT
                  ( PromotedConsT )
                  ( repType r ) )
                  ( b )
              ) PromotedNilT sr
        in  AppT
              ( PromotedT 'TupleRep )
              ( sr_ty )
    SumRep sr         ->
        let sr_ty = foldr (\ r b ->
                AppT ( AppT
                  ( PromotedConsT )
                  ( repType r ) )
                  ( b )
              ) PromotedNilT sr
        in  AppT
              ( PromotedT 'SumRep )
              ( sr_ty )
    VecRep count elem ->
      ( AppT ( AppT
          ( PromotedT 'VecRep )
          ( countType count ) )
          ( elemType elem ) )
    BoxedRep levity   ->
      ( AppT
          ( PromotedT 'BoxedRep )
          ( levityType levity ) )


-- * Type-to-term demotion

-- ** Type-to-term demotion

{- | Type-to-term demotion -}
class Rep (r :: RuntimeRep) where
    {- | Ambiguous in 'r'\;
        requires explicit type application
    -}
    repTerm :: RuntimeRep
    repOf :: forall (a :: TYPE r) -> RuntimeRep

-- ** TemplateHaskell generation of representation type-to-term demotion

{- | Given argument @r@,
    generates type-to-term demotion of promoted instances of @r@
    and of types of type @'TYPE' r@\;
    morally equivalent to the @CPP@ macro
    @
        #define DERIVE_REP(r)                           \
        instance Rep (r) where                          \
            {-# INLINE CONLIKE repAsTerm #-}            \
          ; repTerm :: RuntimeRep                       \
          ; repTerm = r                                 \
          ; {-# INLINE CONLIKE repOfType #-}            \
          ; repOf :: forall (a :: TYPE r) -> RuntimeRep \
          ; repOf = \ _ -> r
    @
    Requires @-XAllowAmbiguousTypes -XDataKinds -XInstanceSigs -XPolyKinds -XRequiredTypeArguments -XScopedTypeVariables@\;
    if @repGrp r@ is not 'Prim' then requires @-XFlexibleInstances@.
    Throws @-Wunused-foralls@ and @-Worphans@.
-}
declareRep :: RuntimeRep -> Q Dec
declareRep = \ r -> do
    guardExts
      ( "\'Data.RuntimeRep.declareRep\'" )
      [ AllowAmbiguousTypes
      , DataKinds
      , InstanceSigs
      , PolyKinds
      , RequiredTypeArguments
      , ScopedTypeVariables ]
    case repGrp r of
        Prim -> pure ()
        _    -> guardExts
          ( "@Data.RuntimeRep.declareRep (" <> show r <> ")@" )
          [ FlexibleInstances ]
    let r_ty = repType r
    guardNoInstance
      ( "\'Data.RuntimeRep.declareRep\'" )
      ( ''Rep )
      [ r_ty ]
    let r_ex = repExp r
    a_nm <- newName "a"
    pure
      ( InstanceD
          ( Nothing )
          [ ]
          ( AppT
              ( ConT ''Rep )
              ( r_ty ) )
          [ ValD
              ( VarP 'repTerm )
              ( NormalB ( r_ex ) )
              [ ]
          , SigD
              ( 'repTerm )
              ( ConT ''RuntimeRep )
          , PragmaD ( InlineP
              ( 'repTerm )
              ( Inline )
              ( ConLike )
              ( AllPhases ) )
          , ValD
              ( VarP 'repOf )
              ( NormalB ( LamE
                  [ WildP ]
                  ( r_ex ) ) )
              [ ]
          , SigD
              ( 'repOf )
              ( ForallVisT
                  [ KindedTV
                      ( a_nm )
                      ( )
                      ( AppT
                          ( ConT ''TYPE )
                          ( r_ty ) ) ]
                  ( ConT ''RuntimeRep ) )
          , PragmaD ( InlineP
              ( 'repOf )
              ( Inline )
              ( ConLike )
              ( AllPhases ) ) ] )