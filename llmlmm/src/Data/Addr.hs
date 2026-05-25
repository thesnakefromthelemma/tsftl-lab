{-# LANGUAGE Haskell2010
  , GHCForeignImportPrim
  , InstanceSigs
  , KindSignatures
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskell
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
    @-Worphans@ is disabled so that we can
    generate 'Addrable' instances (defined in "Data.Addr.TH")
    in this module ("Data.Addr") for types defined in "GHC.Exts"\;
    this is safe because 'Addrable' is exported outside this package solely
    by this module.
-}
{-# OPTIONS_GHC
    -Wall
    -Wno-inaccessible-code
    -Wno-overlapping-patterns
    -Wno-orphans
#-}

{- | 'State#'-parametrized machine addresses -}
module Data.Addr
  ( -- * 'State#'-parametrized machine addresses
    Addr#
    -- * Manual (i.e., non-GC, foreign heap) bytearray (a/rea/dea)llocation via 'Addr#'s
  , allocAddrBytes#
  , allocAddrBytesAligned#
  , callocAddrBytes#
  , callocAddrBytesAligned#
  , reallocAddrBytes#
  , freeAddr#
    -- * Machine 'Addr#' arithmetic
  , eqAddr#
  , neAddr#
  , geAddr#
  , gtAddr#
  , leAddr#
  , ltAddr#
  , plusAddrBytes#
  , minusAddrBytes#
  , remAddrBytes#
    -- * Prefetching via 'Addr#'s
  , prefetchAddr0#
  , prefetchAddr1#
  , prefetchAddr2#
  , prefetchAddr3#
    -- * Bulk byte manipulations via 'Addr#'s
  , setAddrBytesSigned#
  , setAddrBytesUnsigned#
  , copyAddrBytes#
  , copyAddrNonOverlappingBytes#
    -- * Writing/reading off 'Addr#'s
  , Addrable
      ( writeAddr#
      , readAddr#
      )
  , writeCharOffAddr#
  , writeWideCharOffAddr#
  , readCharOffAddr#
  , readWideCharOffAddr#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import Prelude hiding
  ( elem )

{-import qualified Prelude
  ( elem )-}

import GHC.Exts
  ( pattern Int8Rep
  , pattern Int16Rep
  , pattern Int32Rep
  , pattern Int64Rep
  , pattern IntRep
  , pattern Word8Rep
  , pattern Word16Rep
  , pattern Word32Rep
  , pattern Word64Rep
  , pattern WordRep
  , pattern FloatRep
  , pattern DoubleRep
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
  , pattern DoubleElemRep
  , pattern VecRep-}
  , Int8#
  , Int16#
  , Int32#
  , Int64#
  , Int#
  , Word8#
  , Word16#
  , Word32#
  , Word64#
  , Word#
  , Char#
  , Float#
  , Double#
{-, Int8X16#
  , Int16X8#
  , Int32X4#
  , Int64X2#
  , Int8X32#
  , Int16X16#
  , Int32X8#
  , Int64X4#
  , Int8X64#
  , Int16X32#
  , Int32X16#
  , Int64X8#
  , Word8X16#
  , Word16X8#
  , Word32X4#
  , Word64X2#
  , Word8X32#
  , Word16X16#
  , Word32X8#
  , Word64X4#
  , Word8X64#
  , Word16X32#
  , Word32X16#
  , Word64X8#
  , FloatX4#
  , FloatX8#
  , FloatX16#
  , DoubleX2#
  , DoubleX4#
  , DoubleX8#-}
  , State#
  )

import qualified GHC.Exts as GHC
  ( eqAddr#
  , neAddr#
  , geAddr#
  , gtAddr#
  , leAddr#
  , ltAddr#
  , plusAddr#
  , minusAddr#
  , remAddr#
  , prefetchAddr0#
  , prefetchAddr1#
  , prefetchAddr2#
  , prefetchAddr3#
  , writeInt8OffAddr#
  , writeInt16OffAddr#
  , writeInt32OffAddr#
  , writeInt64OffAddr#
  , writeIntOffAddr#
  , writeWord8OffAddr#
  , writeWord16OffAddr#
  , writeWord32OffAddr#
  , writeWord64OffAddr#
  , writeWordOffAddr#
  , writeFloatOffAddr#
  , writeDoubleOffAddr#  
{-, writeInt8X16OffAddr#
  , writeInt16X8OffAddr#
  , writeInt32X4OffAddr#
  , writeInt64X2OffAddr#
  , writeInt8X32OffAddr#
  , writeInt16X16OffAddr#
  , writeInt32X8OffAddr#
  , writeInt64X4OffAddr#
  , writeInt8X64OffAddr#
  , writeInt16X32OffAddr#
  , writeInt32X16OffAddr#
  , writeInt64X8OffAddr#
  , writeWord8X16OffAddr#
  , writeWord16X8OffAddr#
  , writeWord32X4OffAddr#
  , writeWord64X2OffAddr#
  , writeWord8X32OffAddr#
  , writeWord16X16OffAddr#
  , writeWord32X8OffAddr#
  , writeWord64X4OffAddr#
  , writeWord8X64OffAddr#
  , writeWord16X32OffAddr#
  , writeWord32X16OffAddr#
  , writeWord64X8OffAddr#
  , writeFloatX4OffAddr#
  , writeFloatX8OffAddr#
  , writeFloatX16OffAddr#
  , writeDoubleX2OffAddr#
  , writeDoubleX4OffAddr#
  , writeDoubleX8OffAddr#-}
  , writeCharOffAddr#
  , writeWideCharOffAddr#
  , readInt8OffAddr#
  , readInt16OffAddr#
  , readInt32OffAddr#
  , readInt64OffAddr#
  , readIntOffAddr#
  , readWord8OffAddr#
  , readWord16OffAddr#
  , readWord32OffAddr#
  , readWord64OffAddr#
  , readWordOffAddr#
  , readFloatOffAddr#
  , readDoubleOffAddr#  
{-, readInt8X16OffAddr#
  , readInt16X8OffAddr#
  , readInt32X4OffAddr#
  , readInt64X2OffAddr#
  , readInt8X32OffAddr#
  , readInt16X16OffAddr#
  , readInt32X8OffAddr#
  , readInt64X4OffAddr#
  , readInt8X64OffAddr#
  , readInt16X32OffAddr#
  , readInt32X16OffAddr#
  , readInt64X8OffAddr#
  , readWord8X16OffAddr#
  , readWord16X8OffAddr#
  , readWord32X4OffAddr#
  , readWord64X2OffAddr#
  , readWord8X32OffAddr#
  , readWord16X16OffAddr#
  , readWord32X8OffAddr#
  , readWord64X4OffAddr#
  , readWord8X64OffAddr#
  , readWord16X32OffAddr#
  , readWord32X16OffAddr#
  , readWord64X8OffAddr#
  , readFloatX4OffAddr#
  , readFloatX8OffAddr#
  , readFloatX16OffAddr#
  , readDoubleX2OffAddr#
  , readDoubleX4OffAddr#
  , readDoubleX8OffAddr#-}
  , readCharOffAddr#
  , readWideCharOffAddr#
  )

-- ++ (internal)

import Data.RuntimeRep
  ( pattern Prim
  , pattern Lim
  , pattern Vec
  , pattern Box
{-, repBytes
  , supportedSIMDBytes-}
  )

import Data.Addr.TH
  ( Addr#
    ( Addr# )
  , Addrable
      ( writeAddr#
      , readAddr#
      )
  , deriveAddrable
  ) 


-- * 'State#'-parametrized machine addresses


-- * Manual (i.e., non-GC, foreign heap) bytearray (a/rea/dea)llocation via 'Addr#'s

{- | Given argument @n@,
    returns the 'State#' action
    allocating @n@ bytes on the foreign heap,
    its result the machine address of the allocation\;
    wraps a @ccall@ to @malloc@
-}
foreign import prim "allocAddrBytesPrimOp"
    allocAddrBytes# :: forall s. Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @n@, @d@,
    returns the 'State#' action
    allocating @n@ bytes of alignment @d@ bytes on the foreign heap,
    its result the machine address of the allocation\;
    wraps a @ccall@ to @alloc_aligned@\;
    assumes that @n@ is a multiple of @d@
-}
foreign import prim "allocAddrBytesAlignedPrimOp"
    allocAddrBytesAligned# :: forall s. Int# -> Int# -> State# s -> (# State# s, Addr# s #)

{- | Given argument @n@,
    returns the 'State#' action
    allocating @n@ zeroed bytes on the foreign heap,
    its result the machine address of the allocation\;
    wraps a @ccall@ to @calloc@
-}
foreign import prim "callocAddrBytesPrimOp"
    callocAddrBytes# :: forall s. Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @n@, @d@,
    returns the 'State#' action
    allocating @n@ zeroed bytes of alignment @d@ bytes on the foreign heap,
    its result the machine address of the allocation\;
    wraps a @ccall@ to @alloc_aligned@ and a @prim@ call to @memset@\;
    assumes that @n@ is a multiple of @d@
-}
foreign import prim "callocAddrBytesAlignedPrimOp"
    callocAddrBytesAligned# :: forall s. Int# -> Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    resizing @p@\'s allocation to @n@ bytes,
    its result the machine address of the resized allocation\;
    wraps a @ccall@ to @realloc@
-}
foreign import prim "reallocAddrBytesPrimOp"
    reallocAddrBytes# :: forall s. Addr# s -> Int# -> State# s -> (# State# s, Addr# s #)

{- | Given argument @p@,
    returns the 'State#' action
    freeing @p@\'s allocation\;
    wraps a @ccall@ to @free@
-}
foreign import prim "freeAddrPrimOp"
    freeAddr# :: forall s. Addr# s -> State# s -> State# s


-- * Basic bulk byte manipulations

{- | Given arguments @p@, @n@, @c@
    returns the 'State#' action
    setting the first @n@ bytes off @p@ to @c@\;
    wraps a @prim@ call to @memset@
-}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesSigned# :: forall s. Addr# s -> Int# -> Int8# -> State# s -> State# s

{- | Given arguments @p@, @n@, @c@
    returns the 'State#' action
    setting the first @n@ bytes off @p@ to @c@\;
    wraps a @prim@ call to @memset@
-}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesUnsigned# :: forall s. Addr# s -> Int# -> Word8# -> State# s -> State# s

{- | Given arguments @p_src@, @p_dst@ @n@,
    returns the 'State#' action
    copying the first @n@ bytes off @p_src@ to the first @n@ bytes off @p_dst@,
    where the two ranges may overlap\;
    wraps a @prim@ call to @memmove@
-}
foreign import prim "copyAddrBytesPrimOp"
    copyAddrBytes# :: forall s. Addr# s -> Addr# s -> Int# -> State# s -> State# s

{- | Given arguments @p_src@, @p_dst@ @n@,
    returns the 'State#' action
    copying the first @n@ bytes off @p_src@ to the first @n@ bytes off @p_dst@
    where the two ranges may not overlap\;
    wraps a @prim@ call to @memcpy@
-}
foreign import prim "copyAddrNonOverlappingBytesPrimOp"
    copyAddrNonOverlappingBytes# :: forall s. Addr# s -> Addr# s -> Int# -> State# s -> State# s


-- * 'Addr#' arithmetic

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is equal to @p1@ and @0#@ otherwise
-}
{-# INLINE eqAddr# #-}
eqAddr# :: forall s. Addr# s -> Addr# s -> Int#
eqAddr# = \ (Addr# a0) (Addr# a1) -> GHC.eqAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is not equal to @p1@ and @0#@ otherwise
-}
{-# INLINE neAddr# #-}
neAddr# :: forall s. Addr# s -> Addr# s -> Int#
neAddr# = \ (Addr# a0) (Addr# a1) -> GHC.neAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE geAddr# #-}
geAddr# :: forall s. Addr# s -> Addr# s -> Int#
geAddr# = \ (Addr# a0) (Addr# a1) -> GHC.geAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than @p1@ and @0#@ otherwise
-}
{-# INLINE gtAddr# #-}
gtAddr# :: forall s. Addr# s -> Addr# s -> Int#
gtAddr# = \ (Addr# a0) (Addr# a1) -> GHC.gtAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE leAddr# #-}
leAddr# :: forall s. Addr# s -> Addr# s -> Int#
leAddr# = \ (Addr# a0) (Addr# a1) -> GHC.leAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than @p1@ and @0#@ otherwise
-}
{-# INLINE ltAddr# #-}
ltAddr# :: forall s. Addr# s -> Addr# s -> Int#
ltAddr# = \ (Addr# a0) (Addr# a1) -> GHC.ltAddr# a0 a1

{- | Given arguments @p@, @n@,
    returns the machine address an offset of @n@ bytes from @p@
-}
{-# INLINE plusAddrBytes# #-}
plusAddrBytes# :: forall s. Addr# s -> Int# -> Addr# s
plusAddrBytes# = \ (Addr# a) n -> Addr# (GHC.plusAddr# a n)

{- | Given arguments @p0@, @p1@,
    returns the offset of @p0@ from @p1@ in bytes
-}
{-# INLINE minusAddrBytes# #-}
minusAddrBytes# :: forall s. Addr# s -> Addr# s -> Int#
minusAddrBytes# = \ (Addr# a0) (Addr# a1) -> GHC.minusAddr# a0 a1

{- | Given arguments @p@, @m@,
    returns the remainder in bytes when @p@ is divided by @m@
-}
{-# INLINE remAddrBytes# #-}
remAddrBytes# :: forall s. Addr# s -> Int# -> Int#
remAddrBytes# = \ (Addr# a) m -> GHC.remAddr# a m


-- * Prefetching via 'Addr#'s

{- | Given argument @p@,
    prefetches @p@ to a register
-}
{-# INLINE prefetchAddr0# #-}
prefetchAddr0# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr0# = \ (Addr# a) -> GHC.prefetchAddr0# a

{- | Given argument @p@,
    prefetches @p@ to the L1 cache
-}
{-# INLINE prefetchAddr1# #-}
prefetchAddr1# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr1# = \ (Addr# a) -> GHC.prefetchAddr1# a

{- | Given argument @p@,
    prefetches @p@ to the L2 cache
-}
{-# INLINE prefetchAddr2# #-}
prefetchAddr2# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr2# = \ (Addr# a) -> GHC.prefetchAddr2# a

{- | Given argument @p@,
    prefetches @p@ to the L3 cache
-}
{-# INLINE prefetchAddr3# #-}
prefetchAddr3# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr3# = \ (Addr# a) -> GHC.prefetchAddr3# a


-- * Writing/reading off 'Addr#'s

{- | Instantiates 'Addrable' for various 'RuntimeRep's\;
    as the support for SIMD vectors is platform-dependent
    (and not yet fully implemented in GHCi),
    that portion is commented out for now
    (although it otherwise works)
-}
$(pure $ do
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
              , FloatRep
              , DoubleRep ]
            [ deriveAddrable r ]
        Lim  -> []
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
            case Prelude.elem (I# (repBytes r)) supportedSIMDBytes of
                True  -> [ deriveAddrable r ]
                False -> [ ]-}
        Box  -> []
  )

{- | Given arguments @p@, @n@, @c@,
    returns the 'State#' action
    writing @c@ to @p@ at an offset of @n@ bytes,
    where @c@ is assumed to be @1@ byte
-}
{-# INLINE writeCharOffAddr# #-}
writeCharOffAddr# :: forall s. Addr# s -> Int# -> Char# -> State# s -> State# s
writeCharOffAddr# = \ (Addr# a) -> GHC.writeCharOffAddr# a

{- | Given arguments @p@, @n@, @c@,
    returns the 'State#' action
    writing @c@ to @p@ at an offset of @4 * n@ bytes,
    where @c@ is assumed to be @4@ bytes
-}
{-# INLINE writeWideCharOffAddr# #-}
writeWideCharOffAddr# :: forall s. Addr# s -> Int# -> Char# -> State# s -> State# s
writeWideCharOffAddr# = \ (Addr# a) -> GHC.writeWideCharOffAddr# a

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading from @p@ at an offset of @n@ bytes,
    where @c@ is assumed to be @1@ byte
-}
{-# INLINE readCharOffAddr# #-}
readCharOffAddr# :: forall s. Addr# s -> Int# -> State# s -> (# State# s, Char# #)
readCharOffAddr# = \ (Addr# a) -> GHC.readCharOffAddr# a

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading from @p@ at an offset of @4 * n@ bytes,
    where @c@ is assumed to be @4@ bytes
-}
{-# INLINE readWideCharOffAddr# #-}
readWideCharOffAddr# :: forall s. Addr# s -> Int# -> State# s -> (# State# s, Char# #)
readWideCharOffAddr# = \ (Addr# a) -> GHC.readWideCharOffAddr# a