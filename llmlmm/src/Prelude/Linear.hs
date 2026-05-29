{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTSyntax
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , StandaloneKindSignatures
  , TemplateHaskell
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , UnboxedTuples
#-}

{-| @-Worphans@ is disabled so that we can
    generate 'Ur', 'Urable', 'Urlike', and 'Supp' instances
    (defined in "Prelude.Linear.TH")
    in this module ("Prelude.Linear")
    for types defined in "GHC.Exts"\;
    this is safe because these classes
    are exported outside this package solely by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-orphans
#-}

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * "Prelude.Linear" exports fold/build-fusible linear 'Data.List's

  * GHC: More SIMD support (cf. Issue #25030)

  * Exports cased on more host archs (cf. "GHC.Platform.ArchOS")

  * @#if SIMD ... #endif@ removed
-}

{- | Miscellaneous linear utilities -}
module Prelude.Linear
  ( -- * Representation-polymorphic unrestricted-like interfaces
    -- ** Representation-polymorphic strict unrestricted modality
    Urable
      ( Ur
      , ur
      , evUr
      )
  , deriveUrable
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
    -- ** Representation-polymorphic interface to unrestricted-like types 
  , Urlike
      ( rep0
      , rep1
      , rep2
      , rep3
      , rep4
      , rep5
      , rep6
      , rep7
      , rep8
#if FULL
      , rep9
      , rep10
      , rep11
      , rep12
      , rep13
      , rep14
      , rep15
      , rep16
      , rep17
      , rep18
      , rep19
      , rep20
      , rep21
      , rep22
      , rep23
      , rep24
      , rep25
      , rep26
      , rep27
      , rep28
      , rep29
      , rep30
      , rep31
      , rep32
      , rep33
      , rep34
      , rep35
      , rep36
      , rep37
      , rep38
      , rep39
      , rep40
      , rep41
      , rep42
      , rep43
      , rep44
      , rep45
      , rep46
      , rep47
      , rep48
      , rep49
      , rep50
      , rep51
      , rep52
      , rep53
      , rep54
      , rep55
      , rep56
      , rep57
      , rep58
      , rep59
      , rep60
      , rep61
      , rep62
      , rep63
      , rep64
#endif
      )
  , deriveUrlike
    -- ** Representation polymorphic interface to linearly suppressible types
  , Supp
      ( supp )
  , deriveSupp
    -- * Kind and multiplicity-polymorphic 'Prelude.($)' operator
  , ($)
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( ($) )

import qualified Prelude as NL
  ( ($) )

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
      , VecRep
      , BoxedRep )
  , TYPE
  , Multiplicity
  )

-- ++ (internal)

import Data.RuntimeRep
  ( pattern Prim
  , pattern Lim
  , pattern Vec
  , pattern Box
#if SIMD
  , supportedSIMDType
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
  , declareUrlikeUnit
  , declareUrlikeUr
  , deriveUrlike
  , Supp
      ( supp )
  , declareSuppViaUrlike
  , deriveSupp
  )


-- * Representation-polymorphic unrestricted-like interfaces

-- ** Representation-polymorphic strict unrestricted modality

{- | Instantiates 'Ur' for various 'RuntimeRep's -}
$(sequence NL.$ do
    let sr = do
            g <-
              [ Prim
              , Lim
              , Vec
              , Box ]
            r <- case g of
                Prim -> do
                    a <-
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
                    [ a ]
                Lim  -> do
                    x <-
                      [ TupleRep
                      , SumRep ]
                    [ x [ ] ]
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
                    case supportedSIMDType e c of
                        True  -> [ VecRep c e ]
                        False -> [ ]
#else
                Vec  -> [ ]
#endif
                Box  -> do
                    l <-
                      [ Unlifted
                      , Lifted ]
                    [ BoxedRep l ]
            [ r ]
    r <- sr
    [ deriveUrable r ]
  )

-- ** Representation-polymorphic interface to unrestricted-like types 

{- | Declares 'Urlike' -}
$( declareUrlike )

{- | Instantiates 'Urlike' for @(# #)@ -}
$(sequence
    [ declareUrlikeUnit ]
  )

{- | Instantiates 'Urlike' for @Ur a@ with @a@ of various 'RuntimeRep's -}
$(sequence NL.$ do
    let sr = do
            g <-
              [ Prim
              , Lim
              , Vec
              , Box ]
            r <- case g of
                Prim -> do
                    a <-
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
                    [ a ]
                Lim  -> do
                    x <-
                      [ TupleRep
                      , SumRep ]
                    [ x [ ] ]
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
                    case supportedSIMDType e c of
                        True  -> [ VecRep c e ]
                        False -> [ ]
#else
                Vec  -> [ ]
#endif
                Box  -> do
                    l <-
                      [ Unlifted
                      , Lifted ]
                    [ BoxedRep l ]
            [ r ]
    r <- sr
    [ declareUrlikeUr r ]
  )

-- ** Representation-polymorphic interface to linearly suppressible types 

{- | Declares 'Supp' via 'Urlike' -}
$(sequence NL.$ do
    let sr = do
            g <-
              [ Prim
              , Lim
              , Vec
              , Box ]
            r <- case g of
                Prim -> do
                    a <-
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
                    [ a ]
                Lim  -> do
                    x <-
                      [ TupleRep
                      , SumRep ]
                    [ x [ ] ]
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
                    case supportedSIMDType e c of
                        True  -> [ VecRep c e ]
                        False -> [ ]
#else
                Vec  -> [ ]
#endif
                Box  -> do
                    l <-
                      [ Unlifted
                      , Lifted ]
                    [ BoxedRep l ]
            [ r ]
    s <- sr
    [ declareSuppViaUrlike s ]
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