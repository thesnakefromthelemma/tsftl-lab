{-# LANGUAGE Haskell2010
  , CPP
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
    generate 'Ur', 'Urable', 'Urlike', and 'Supp' instances
    (defined in "Prelude.Linear.TH")
    in this module ("Prelude.Linear")
    for types defined in "GHC.Exts"\;
    this is safe because these classes
    are exported outside this package solely by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
    -Wno-orphans
#-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * Implement linear fold/build-fusible 'Data.List's

  * Upgrade GHC's SIMD support (cf. issue #25030)

  * Case SIMD support on more host archs (cf. "GHC.Platform.ArchOS")
-}

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
#if SIMD
#if defined(x86_64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , pattern UrInt8X16#
  , pattern UrInt16X8#
  , pattern UrInt32X4#
  , pattern UrInt64X2#
  , pattern UrWord8X16#
  , pattern UrWord16X8#
  , pattern UrWord32X4#
  , pattern UrWord64X2#
  , pattern UrFloatX4#
  , pattern UrDoubleX2#
  , pattern UrInt8X32#
  , pattern UrInt16X16#
  , pattern UrInt32X8#
  , pattern UrInt64X4#
  , pattern UrWord8X32#
  , pattern UrWord16X16#
  , pattern UrWord32X8#
  , pattern UrWord64X4#
  , pattern UrFloatX8#
  , pattern UrDoubleX4#
  , pattern UrInt8X64#
  , pattern UrInt16X32#
  , pattern UrInt32X16#
  , pattern UrInt64X8#
  , pattern UrWord8X64#
  , pattern UrWord16X32#
  , pattern UrWord32X16#
  , pattern UrWord64X8#
  , pattern UrFloatX16#
  , pattern UrDoubleX8#
#else
  , pattern UrInt8X16#
  , pattern UrInt16X8#
  , pattern UrInt32X4#
  , pattern UrInt64X2#
  , pattern UrWord8X16#
  , pattern UrWord16X8#
  , pattern UrWord32X4#
  , pattern UrWord64X2#
  , pattern UrFloatX4#
  , pattern UrDoubleX2#
#endif
#elif defined(aarch64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , pattern UrInt8X16#
  , pattern UrInt16X8#
  , pattern UrInt32X4#
  , pattern UrInt64X2#
  , pattern UrWord8X16#
  , pattern UrWord16X8#
  , pattern UrWord32X4#
  , pattern UrWord64X2#
  , pattern UrFloatX4#
  , pattern UrDoubleX2#
#else
#endif
#endif
#endif
  , pattern Ur#
  , pattern Ur
    -- * Representation-polymorphic unrestricted-like types 
  , Urlike (..)
    -- * Kind and multiplicity-polymorphic 'Prelude.($)' operator
  , ($)
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( ($)
#if SIMD
  , elem
#endif
  )

import qualified Prelude as NL
  ( ($)
#if SIMD
  , elem
#endif
  )

import GHC.Exts
  (
#if SIMD
    pattern Vec2
  , pattern Vec4
  , pattern Vec8
  , pattern Vec16
  , pattern Vec32
  , pattern Vec64
  , pattern Int8ElemRep
  , pattern Int16ElemRep
  , pattern Int32ElemRep
  , pattern Int64ElemRep
  , pattern Word8ElemRep
  , pattern Word16ElemRep
  , pattern Word32ElemRep
  , pattern Word64ElemRep
  , pattern FloatElemRep
  , pattern DoubleElemRep
  ,
#endif
    pattern Unlifted
  , pattern Lifted
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
#if SIMD
      , VecRep
#endif
      , BoxedRep )
  , TYPE
  , Multiplicity
#if SIMD
  , pattern I#
#endif
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
#if SIMD
  , repBytes
  , supportedSIMDBytes
#endif
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
  )


-- * Representation-polymorphic interface to strict unrestricted modality

{- | Instantiates 'Ur' for various 'RuntimeRep's -}
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
#if SIMD
        Vec  -> do
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
                False -> [ ]
#else
        Vec -> [ ]
#endif
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

{- | Instantiates 'Urlike' for @Ur a@ with @a@ of various 'RuntimeRep's -}
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
#if SIMD
        Vec  -> do
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
                False -> [ ]
#else
        Vec  -> [ ]
#endif
        Box  -> do
            l <-
              [ Unlifted
              , Lifted ]
            let r = BoxedRep l
            [ declareUrlikeUr r ]
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