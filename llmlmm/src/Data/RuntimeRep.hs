{-# LANGUAGE Haskell2010
  , BangPatterns
  , CPP
  , GADTSyntax
  , LambdaCase
  , MagicHash
  , PatternSynonyms
  , TemplateHaskellQuotes
#-}

{-# OPTIONS_GHC -Wall #-}

#include "MachDeps.h"

{- | Miscellaneous 'RuntimeRep' utilities -}
module Data.RuntimeRep
  ( -- * Fundamental representation groups
    RepGrp
      ( Prim
      , Lim
      , Vec
      , Box
      )
    -- * TemplateHaskell promotion
  , repGrp
  , elemType
  , countType
  , repType
    -- * Size information
  , elemBytes
  , countSize
  , repBytes
  , supportedSIMDBytes
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
  , pattern PromotedT
  , pattern PromotedNilT
  , pattern PromotedConsT
  , pattern UnboxedTupleT
  , pattern UnboxedSumT
  , pattern ConT
  , pattern AppT
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

{- | Given argument @e@,
    returns the promoted type of @e@ as a TemplateHaskell expression
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
    returns the promoted type of @c@ as a TemplateHaskell expression
-}
countType :: VecCount -> Type
countType = \case
    Vec2  -> PromotedT 'Vec2
    Vec4  -> PromotedT 'Vec4
    Vec8  -> PromotedT 'Vec8
    Vec16 -> PromotedT 'Vec16
    Vec32 -> PromotedT 'Vec32
    Vec64 -> PromotedT 'Vec64

{- | Given argument @r@,
    returns the promoted type of @r@ as a TemplateHaskell expression
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
        AppT
          ( PromotedT 'TupleRep )
          ( foldr (\ r b ->
                AppT ( AppT
                  ( PromotedConsT )
                  ( repType r ) )
                  ( b )
              ) PromotedNilT sr )
    SumRep sr         ->
        AppT
          ( PromotedT 'SumRep )
          ( foldr (\ r b ->
                AppT ( AppT
                  ( PromotedConsT )
                  ( repType r ) )
                  ( b )
              ) PromotedNilT sr )
    VecRep count elem ->
        AppT ( AppT
          ( PromotedT 'VecRep )
          ( countType count ) )
          ( elemType elem )
    BoxedRep Unlifted ->
        AppT
          ( PromotedT 'BoxedRep )
          ( PromotedT 'Unlifted )
    BoxedRep Lifted   ->
        AppT
          ( PromotedT 'BoxedRep )
          ( PromotedT 'Lifted )


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
    AddrRep           -> SIZEOF_HSWORD#
    FloatRep          -> SIZEOF_FLOAT#
    DoubleRep         -> SIZEOF_DOUBLE#
    TupleRep sr       -> case sum $ map (\ r -> I# (repBytes r)) sr of I# b -> b
    SumRep sr         -> case sum $ map (\ r -> I# (repBytes r)) sr of I# b -> b
    VecRep count elem -> elemBytes elem *# countSize count
    BoxedRep _        -> error "\'Data.RuntimeRep.Extra.repBytes\' not defined for boxed representations"

{- | Currently supported SIMD vector sizes in bytes -}
supportedSIMDBytes :: [Int]
supportedSIMDBytes =
  [ I# 16#
  , I# 32#
  , I# 64# ]


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