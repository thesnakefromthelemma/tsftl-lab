{-# LANGUAGE Haskell2010
  , AllowAmbiguousTypes
  , BangPatterns
  , CPP
  , DataKinds
  , FlexibleInstances
  , InstanceSigs
  , LambdaCase
  , MagicHash
  , PatternSynonyms
  , PolyKinds
  , RequiredTypeArguments
  , ScopedTypeVariables
  , TemplateHaskell
#-}

{-| @-Wunused-foralls@ is disabled so that we can
    define method 'ofType' of class 'Rep'.
    @-Worphans@ is disabled so that we can
    generate 'Rep' instances
    (defined in "Data.RuntimeRep.TH")
    in this module
    ("Data.RuntimeRep")
    for terms of 'RuntimeRep'
    (defined in "GHC.Exts")\;
    this is safe because 'Rep'
    is exported outside this package solely by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-unused-foralls
    -Wno-orphans
#-}

#include "MachDeps.h"

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~
 
  * The SIMD alignment story in 'repAlignment' is correct

  * GHC: More SIMD support (cf. Issue #25030)

  * 'Rep' instances and 'supportedSIMDType' case on more host archs (cf. "GHC.Platform.ArchOS")
-}

{- | Miscellaneous 'RuntimeRep' utilities -}
module Data.RuntimeRep
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
    -- * Size information
  , elemBytes
  , countSize
  , repBytes
  , repAlignment
    -- * SIMD information
  , supportedSIMDType
    -- * Name information
  , elemStem
  , countStem
  , repStem
    -- * Standard instances
  , repEg
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( elem )

#if defined(x86_64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
import qualified Prelude
  ( elem )
#endif
#elif defined(aarch64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
import qualified Prelude
  ( elem )
#else
#endif
#else
#endif

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
  , pattern Unlifted
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
  , Int#
  , pattern I#
  , (*#)
  )

-- ++ From template-haskell >= 2.23 && < 2.25

import Language.Haskell.TH
  ( mkName
  , Type
  , pattern UnboxedTupleT
  , pattern UnboxedSumT
  , pattern ConT
  , pattern AppT
  )

-- ++ (internal)

import Data.RuntimeRep.TH
  ( RepGrp
      ( Prim
      , Lim
      , Vec
      , Box
      )
  , repGrp
  , elemExp
  , countExp
  , levityExp
  , repExp
  , elemType
  , countType
  , levityType
  , repType
  , Rep
      ( repTerm
      , repOf
      )
  , declareRep
  )


-- * Type-to-term demotion

{- | Instantiates 'Ur' for various 'RuntimeRep's -}
$(sequence $ do
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
                    [ VecRep c e ]
                Box  -> do
                    l <-
                      [ Unlifted
                      , Lifted ]
                    [ BoxedRep l ]
            [ r ]
    r <- sr
    [ declareRep r ]
  )

-- * Size information

{- | Given argument @e@,
    returns the size of a SIMD vector element of representation @e@ in bytes
-}
elemBytes :: VecElem -> Int#
elemBytes = \case
    Int8ElemRep   -> SIZEOF_INT8#
    Int16ElemRep  -> SIZEOF_INT16#
    Int32ElemRep  -> SIZEOF_INT32#
    Int64ElemRep  -> SIZEOF_INT64#
    Word8ElemRep  -> SIZEOF_WORD8#
    Word16ElemRep -> SIZEOF_WORD16#
    Word32ElemRep -> SIZEOF_WORD32#
    Word64ElemRep -> SIZEOF_WORD64#
    FloatElemRep  -> SIZEOF_FLOAT#
    DoubleElemRep -> SIZEOF_DOUBLE#

{- | Given argument @c@,
    returns the number of elements in a SIMD vector of shape @c@
-}
countSize :: VecCount -> Int#
countSize = \case
    Vec2  -> 2#
    Vec4  -> 4#
    Vec8  -> 8#
    Vec16 -> 16#
    Vec32 -> 32#
    Vec64 -> 64#

{- | Given argument @r@,
    returns the size of a term of representation @r@ in bytes
-}
repBytes :: RuntimeRep -> Int#
repBytes = \case
    Int8Rep           -> SIZEOF_INT8#
    Int16Rep          -> SIZEOF_INT16#
    Int32Rep          -> SIZEOF_INT32#
    Int64Rep          -> SIZEOF_INT64#
    IntRep            -> SIZEOF_HSINT#
    Word8Rep          -> SIZEOF_WORD8#
    Word16Rep         -> SIZEOF_WORD16#
    Word32Rep         -> SIZEOF_WORD32#
    Word64Rep         -> SIZEOF_WORD64#
    WordRep           -> SIZEOF_HSWORD#
    AddrRep           -> SIZEOF_VOID_P#
    FloatRep          -> SIZEOF_FLOAT#
    DoubleRep         -> SIZEOF_DOUBLE#
    TupleRep sr       -> case sum $ map (\ r -> I# (repBytes r)) sr of I# b -> b
    SumRep sr         -> case sum $ map (\ r -> I# (repBytes r)) sr of I# b -> b
    VecRep count elem -> elemBytes elem *# countSize count
    BoxedRep _        -> error "\'Data.RuntimeRep.Extra.repBytes\' not defined for boxed representations"

{- | Given argument @r@,
    returns the alignment of a term of representation @r@ in bytes
-}
repAlignment :: RuntimeRep -> Int#
repAlignment = \case
    Int8Rep           -> ALIGNMENT_INT8#
    Int16Rep          -> ALIGNMENT_INT16#
    Int32Rep          -> ALIGNMENT_INT32#
    Int64Rep          -> ALIGNMENT_INT64#
    IntRep            -> ALIGNMENT_HSINT#
    Word8Rep          -> ALIGNMENT_WORD8#
    Word16Rep         -> ALIGNMENT_WORD16#
    Word32Rep         -> ALIGNMENT_WORD32#
    Word64Rep         -> ALIGNMENT_WORD64#
    WordRep           -> ALIGNMENT_HSWORD#
    AddrRep           -> ALIGNMENT_VOID_P#
    FloatRep          -> ALIGNMENT_FLOAT#
    DoubleRep         -> ALIGNMENT_DOUBLE#
    TupleRep _        -> error "\'Data.RuntimeRep.Extra.repAlignment\' not defined for unboxed tuple representations"
    SumRep _          -> error "\'Data.RuntimeRep.Extra.repAlignment\' not defined for unoxed sum representations"
    VecRep count elem -> elemBytes elem *# countSize count -- is this sound?
    BoxedRep _        -> error "\'Data.RuntimeRep.Extra.repAlignment\' not defined for boxed representations"


-- * SIMD information

{- | Currently supported SIMD vector types -}
supportedSIMDType :: VecElem -> VecCount -> Bool
supportedSIMDType =
#if defined(x86_64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
    \ e c -> Prelude.elem (I# $ elemBytes e *# countSize c)
      [ I# 16#
      , I# 32#
      , I# 64# ]
#else
    \ e c -> Prelude.elem (I# $ elemBytes e *# countSize c)
      [ I# 16# ]
#endif
#elif defined(aarch64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
    \ e c -> Prelude.elem (I# $ elemBytes e *# countSize c)
      [ I# 16# ]
#else
    \ _ _ -> False
#endif
#else
    \ _ _ -> False
#endif


-- * Name information

{- | 'VecElem' infix -}
elemStem :: VecElem -> String
elemStem = \case
    Int8ElemRep   -> "Int8"
    Int16ElemRep  -> "Int16"
    Int32ElemRep  -> "Int32"
    Int64ElemRep  -> "Int64"
    Word8ElemRep  -> "Word8"
    Word16ElemRep -> "Word16"
    Word32ElemRep -> "Word32"
    Word64ElemRep -> "Word64"
    FloatElemRep  -> "Float"
    DoubleElemRep -> "Double"

{- | 'VecCount' infix -}
countStem :: VecCount -> String
countStem = \case
    Vec2  -> "2"
    Vec4  -> "4"
    Vec8  -> "8"
    Vec16 -> "16"
    Vec32 -> "32"
    Vec64 -> "64"

{- | 'RuntimeRep' prefix -}
repStem :: RuntimeRep -> String
repStem = \case
    Int8Rep           -> "Int8"
    Int16Rep          -> "Int16"
    Int32Rep          -> "Int32"
    Int64Rep          -> "Int64"
    IntRep            -> "Int"
    Word8Rep          -> "Word8"
    Word16Rep         -> "Word16"
    Word32Rep         -> "Word32"
    Word64Rep         -> "Word64"
    WordRep           -> "Word"
    AddrRep           -> "Addr"
    FloatRep          -> "Float"
    DoubleRep         -> "Double"
    TupleRep []       -> "1"
    TupleRep _        -> error "\'Data.RuntimeRep.Extra.repStem\' not defined for nonempty unboxed tuples"
    SumRep []         -> "0"
    SumRep _          -> error "\'Data.RuntimeRep.Extra.repStem\' not defined for nonempty unboxed sums"
    VecRep count elem -> elemStem elem <> "X" <> countStem count
    BoxedRep _        -> ""


-- * Standard instances

{- | Standard representation instance (if exists) -}
repEg :: RuntimeRep -> Type
repEg = \case
    TupleRep sr -> foldr (\ r k f !n -> k (\ !n' ->
        AppT
          ( f n' )
          ( repEg r )
      ) (n + 1)) ($) sr UnboxedTupleT 0
    SumRep sr   -> foldr (\ r k f !n -> k (\ !n' ->
        AppT
          ( f n' )
          ( repEg r )
      ) (n + 1)) ($) sr UnboxedSumT 0
    BoxedRep _  -> error "\'Data.RuntimeRep.Extra.repEg\' not defined for representations containing boxes"
    r           -> ConT (mkName $ repStem r <> "#")