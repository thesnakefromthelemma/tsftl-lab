{-# LANGUAGE Haskell2010
  , CPP
  , LambdaCase
  , PatternSynonyms
#-}

{-# OPTIONS_GHC -Wall #-}

#include "MachDeps.h"

{- | Miscellaneous 'RuntimeRep' utilities -}
module Data.RuntimeRep.Extra
  ( -- * Miscellaneous 'RuntimeRep' utilities
    elemBytes
  , countSize
  , repBytes
  , supportedSIMDBytes
  , elemStem
  , repStem
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( elem )

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
  , pattern Unlifted
  , pattern Lifted
  )


-- * Miscellaneous 'RuntimeRep' utilities

{- | Given argument @e@,
    returns the size of a SIMD vector element of representation @e@ in bytes
-}
elemBytes :: VecElem -> Int
elemBytes = \case
    Int8ElemRep   -> SIZEOF_INT8
    Int16ElemRep  -> SIZEOF_INT16
    Int32ElemRep  -> SIZEOF_INT32
    Int64ElemRep  -> SIZEOF_INT64
    Word8ElemRep  -> SIZEOF_WORD8
    Word16ElemRep -> SIZEOF_WORD16
    Word32ElemRep -> SIZEOF_WORD32
    Word64ElemRep -> SIZEOF_WORD64
    FloatElemRep  -> SIZEOF_FLOAT
    DoubleElemRep -> SIZEOF_DOUBLE

{- | Given argument @c@,
    returns the number of elements in a SIMD vector of shape @c@
-}
countSize :: VecCount -> Int
countSize = \case
    Vec2  -> 2
    Vec4  -> 4
    Vec8  -> 8
    Vec16 -> 16
    Vec32 -> 32
    Vec64 -> 64

{- | Given argument @r@,
    returns the size of a term of representation @r@ in bytes
-}
repBytes :: RuntimeRep -> Int
repBytes = \case
    Int8Rep           -> SIZEOF_INT8
    Int16Rep          -> SIZEOF_INT16
    Int32Rep          -> SIZEOF_INT32
    Int64Rep          -> SIZEOF_INT64
    IntRep            -> SIZEOF_HSINT
    Word8Rep          -> SIZEOF_WORD8
    Word16Rep         -> SIZEOF_WORD16
    Word32Rep         -> SIZEOF_WORD32
    Word64Rep         -> SIZEOF_WORD64
    WordRep           -> SIZEOF_HSWORD
    AddrRep           -> SIZEOF_HSWORD
    FloatRep          -> SIZEOF_FLOAT
    DoubleRep         -> SIZEOF_DOUBLE
    TupleRep sr       -> sum $ fmap repBytes sr
    SumRep sr         -> sum $ fmap repBytes sr
    VecRep count elem -> elemBytes elem * countSize count
    BoxedRep _        -> error "\'Data.RuntimeRep.Extra.repBytes\' not defined for boxed types"

{- | Currently supported SIMD vector sizes in bytes -}
supportedSIMDBytes :: [Int]
supportedSIMDBytes =
  [ 16
  , 32
  , 64 ]

{- | 'VecElem' prefix -}
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

{- | 'RuntimeRep' prefix -}
repStem :: RuntimeRep -> String
repStem = \case
    Int8Rep           -> "Int8" <> "#"
    Int16Rep          -> "Int16" <> "#"
    Int32Rep          -> "Int32" <> "#"
    Int64Rep          -> "Int64" <> "#"
    IntRep            -> "Int" <> "#"
    Word8Rep          -> "Word8" <> "#"
    Word16Rep         -> "Word16" <> "#"
    Word32Rep         -> "Word32" <> "#"
    Word64Rep         -> "Word64" <> "#"
    WordRep           -> "Word" <> "#"
    AddrRep           -> "Addr" <> "#"
    FloatRep          -> "Float" <> "#"
    DoubleRep         -> "Double" <> "#"
    TupleRep []       -> "1#"
    TupleRep _        -> error "\'Data.RuntimeRep.Extra.repStem\' not defined for nonempty unboxed tuples"
    SumRep []         -> "0#"
    SumRep _          -> error "\'Data.RuntimeRep.Extra.repStem\' not defined for nonempty unboxed sums"
    VecRep count elem -> elemStem elem <> "X" <> show (countSize count) <> "#"
    BoxedRep Unlifted -> "#"
    BoxedRep Lifted   -> ""