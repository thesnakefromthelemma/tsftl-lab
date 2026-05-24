{-# LANGUAGE Haskell2010
  , DataKinds
  , FlexibleInstances
  , GADTSyntax
  , InstanceSigs
  , LambdaCase
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , TemplateHaskell
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , UnboxedTuples
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
    @-Worphans@ is disabled so that we can
    generate 'Ur' and 'Supp' instances (defined in "Prelude.Linear.TH")
    in this module ("Prelude.Linear") for types defined in "GHC.Exts"\;
    this is safe because 'Ur' is exported outside this package solely
    by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-partial-type-signatures
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
    -Wno-orphans
#-}

{- | Miscellaneous linear utilities -}
module Prelude.Linear
  ( -- * Representation-polymorphic interface to strict unrestricted modality
    Urable
      ( Ur
      , ur
      , evUr
      )
  , pattern UrInt8#
  , pattern UrInt16#
  , pattern UrInt32#
  , pattern UrInt64#
  , pattern UrInt#
  , pattern UrWord8#
  , pattern UrWord16#
  , pattern UrWord32#
  , pattern UrWord64#
  , pattern UrWord#
  , pattern UrAddr#
  , pattern UrFloat#
  , pattern UrDouble#
  , pattern Ur1#
  , pattern Ur0#
{-, pattern UrInt8X16#
  , pattern UrInt8X32#
  , pattern UrInt8X64#
  , pattern UrInt16X8#
  , pattern UrInt16X16#
  , pattern UrInt16X32#
  , pattern UrInt32X4#
  , pattern UrInt32X8#
  , pattern UrInt32X16#
  , pattern UrInt64X2#
  , pattern UrInt64X4#
  , pattern UrInt64X8#
  , pattern UrWord8X16#
  , pattern UrWord8X32#
  , pattern UrWord8X64#
  , pattern UrWord16X8#
  , pattern UrWord16X16#
  , pattern UrWord16X32#
  , pattern UrWord32X4#
  , pattern UrWord32X8#
  , pattern UrWord32X16#
  , pattern UrWord64X2#
  , pattern UrWord64X4#
  , pattern UrWord64X8#
  , pattern UrFloatX4#
  , pattern UrFloatX8#
  , pattern UrFloatX16#
  , pattern UrDoubleX2#
  , pattern UrDoubleX4#
  , pattern UrDoubleX8#-}
  , pattern Ur#
  , pattern Ur
    -- * Representation-polymorphic unrestricted-like types 
  , Urlike (..)
    -- * Kind and multiplicity-polymorphic 'Prelude.($)' operator
  , ($)
    -- * Representation-polymorphic unboxed unit suppression
  , Supp
       ( supp )
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( ($)
{-, elem-}
  )

import qualified Prelude as NL
  ( ($)
{-, elem-}
  )

import GHC.Exts
  ( TYPE
{-, pattern Vec2
  , pattern Vec4
  , pattern Vec8
  , pattern Vec16
  , pattern Vec32
  , pattern Vec64-}
{-, pattern Int8ElemRep
  , pattern Int16ElemRep
  , pattern Int32ElemRep
  , pattern Int64ElemRep
  , pattern Word8ElemRep
  , pattern Word16ElemRep
  , pattern Word32ElemRep
  , pattern Word64ElemRep
  , pattern FloatElemRep
  , pattern DoubleElemRep-}
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
{-    , VecRep-}
      , BoxedRep )
  , pattern Unlifted
  , pattern Lifted
{-, pattern I#-}
  , Multiplicity
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( pattern UnboxedTupleT )

-- ++ (internal)

import Data.RuntimeRep
  ( pattern Prim
  , pattern Lim
  , pattern Vec
  , pattern Box
{-, repBytes
  , supportedSIMDBytes-}
  )

import Prelude.Linear.TH
  ( Urable
      ( Ur
      , ur
      , evUr
      )
  , deriveUrable
  , declareUrlike
  , deriveUrlike
  , declareUrlikeUr
  , Supp
      ( supp )
  , deriveSupp
  )


-- * Representation-polymorphic interface to strict unrestricted modality

{- | Instantiates 'Ur' for various 'RuntimeRep's\;
    as the support for SIMD vectors is platform-dependent
    (and not yet fully implemented in GHCi),
    that portion is commented out for now
    (although it otherwise works)
-}
$(pure NL.$ do
    g <-
      [ Prim
      , Lim
      , Vec
      , Box ]
    case g of
        Prim -> do
            r <-
              [ Int8Rep
              , Int16Rep
              , Int32Rep
              , Int64Rep
              , IntRep
              , Word8Rep
              , Word16Rep
              , Word32Rep
              , Word64Rep
              , WordRep
              , AddrRep
              , FloatRep
              , DoubleRep ]
            [ deriveUrable r ]
        Lim  -> do
            v <-
              [ TupleRep
              , SumRep ]
            let r = v []
            [ deriveUrable r ]
        Vec  -> [] {-do
            e <-
              [ Int8ElemRep
              , Int16ElemRep
              , Int32ElemRep
              , Int64ElemRep
              , Word8ElemRep
              , Word16ElemRep
              , Word32ElemRep
              , Word64ElemRep
              , FloatElemRep
              , DoubleElemRep ]         
            c <-
              [ Vec2
              , Vec4
              , Vec8
              , Vec16
              , Vec32
              , Vec64 ]
            let r = VecRep c e
            case NL.elem (I# (repBytes r)) supportedSIMDBytes of
                True  -> [ deriveUrable r ]
                False -> [ ]-}
        Box  -> do
            l <-
              [ Unlifted
              , Lifted ]
            let r = BoxedRep l
            [ deriveUrable r ]
  )


-- * Representation-polymorphic unrestricted-like types 

{- | Declares 'Urlike' -}
$(pure
    [ declareUrlike ]
  )

{- | Instantiates 'Urlike' for @(# #)@ -}
$(pure
    [ deriveUrlike
        ( TupleRep [ ] )
        ( UnboxedTupleT 0 ) ]
  )

{- | Instantiates 'Urlike' for @Ur a@ with
    @a@ of various 'RuntimeRep's\;
    as the support for SIMD vectors is platform-dependent
    (and not yet fully implemented in GHCi),
    that portion is commented out for now
    (although it otherwise works)
-}
$(pure NL.$ do
    g <-
      [ Prim
      , Lim
      , Vec
      , Box ]
    case g of
        Prim -> do
            r <-
              [ Int8Rep
              , Int16Rep
              , Int32Rep
              , Int64Rep
              , IntRep
              , Word8Rep
              , Word16Rep
              , Word32Rep
              , Word64Rep
              , WordRep
              , AddrRep
              , FloatRep
              , DoubleRep ]
            [ declareUrlikeUr r ]
        Lim  -> do
            v <-
              [ TupleRep
              , SumRep ]
            let r = v []
            [ declareUrlikeUr r ]
        Vec  -> [] {-do
            e <-
              [ Int8ElemRep
              , Int16ElemRep
              , Int32ElemRep
              , Int64ElemRep
              , Word8ElemRep
              , Word16ElemRep
              , Word32ElemRep
              , Word64ElemRep
              , FloatElemRep
              , DoubleElemRep ]         
            c <-
              [ Vec2
              , Vec4
              , Vec8
              , Vec16
              , Vec32
              , Vec64 ]
            let r = VecRep c e
            case NL.elem (I# (repBytes r)) supportedSIMDBytes of
                True  -> [ declareUrlikeUr r ]
                False -> [ ]-}
        Box  -> do
            l <-
              [ Unlifted
              , Lifted ]
            let r = BoxedRep l
            [ declareUrlikeUr r ]
  )


-- * Representation-polymorphic unboxed unit suppression

{- | Instantiates 'Supp' for various 'RuntimeRep's\;
    as the support for SIMD vectors is platform-dependent
    (and not yet fully implemented in GHCi),
    that portion is commented out for now
    (although it otherwise works)
-}
$(pure NL.$ do
    g <-
      [ Prim
      , Lim
      , Vec
      , Box ]
    case g of
        Prim -> do
            r <-
              [ Int8Rep
              , Int16Rep
              , Int32Rep
              , Int64Rep
              , IntRep
              , Word8Rep
              , Word16Rep
              , Word32Rep
              , Word64Rep
              , WordRep
              , AddrRep
              , FloatRep
              , DoubleRep ]
            [ deriveSupp r ]
        Lim  -> do
            v <-
              [ TupleRep
              , SumRep ]
            let r = v []
            [ deriveSupp r ]
        Vec  -> [] {-do
            e <-
              [ Int8ElemRep
              , Int16ElemRep
              , Int32ElemRep
              , Int64ElemRep
              , Word8ElemRep
              , Word16ElemRep
              , Word32ElemRep
              , Word64ElemRep
              , FloatElemRep
              , DoubleElemRep ]         
            c <-
              [ Vec2
              , Vec4
              , Vec8
              , Vec16
              , Vec32
              , Vec64 ]
            let r = VecRep c e
            case NL.elem (I# (repBytes r)) supportedSIMDBytes of
                True  -> [ deriveSupp r ]
                False -> [ ]-}
        Box  -> do
            l <-
              [ Unlifted
              , Lifted ]
            let r = BoxedRep l
            [ deriveSupp r ]
  )


-- * Kind and multiplicity-polymorphic 'Prelude.($)' operator

{- | Kind and multiplicity-polymorphic 'Prelude.($)' operator -}
infixr 0 $
{-# INLINE ($) #-}
($) :: forall
    (p :: Multiplicity) (q :: Multiplicity)
    (ra :: RuntimeRep) (rb :: RuntimeRep)
    (a :: TYPE ra) (b :: TYPE rb).
    (a %q -> b) %p -> a %q -> b
($) = \ f -> f