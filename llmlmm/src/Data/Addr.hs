{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleInstances
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
  , ViewPatterns
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

{- 
Note [Future work]
~~~~~~~~~~~~~~~~~~

  * Expose 'GHC.NullAddr#', simplifying 'NullAddr#'

  * Upgrade GHC's SIMD support (cf. issue #25030)

  * Case SIMD support on more host archs (cf. "GHC.Platform.ArchOS")
-}

{- |  @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses -}
module Data.Addr
  ( -- *  @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses
    Addr#
    -- * Manual (i.e., non-GC, foreign heap) bytearray (a/rea/dea)llocation via 'Addr#'s
  , allocAddrBytes#
  , allocAddrBytesAligned#
  , callocAddrBytes#
  , callocAddrBytesAligned#
  , reallocAddrBytes#
  , freeAddr#
    -- * Machine 'Addr#' arithmetic
  , pattern NullAddr#
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
  , writeAddrOffAddr#
  , readCharOffAddr#
  , readWideCharOffAddr#
  , readAddrOffAddr#
    -- * Interoperation with GHC's 'ByteArray#'/'MutableByteArray#'
  , copyMutableByteArrayToAddr#
  , copyByteArrayToAddr#
  , copyAddrToMutableByteArray#
    -- * Concurrency primitives
  , atomicWriteWordAddr#
  , atomicReadWordAddr#
  , fetchXorWordAddr#
  , fetchAndWordAddr#
  , fetchNandWordAddr#
  , fetchOrWordAddr#
  , fetchAddWordAddr#
  , fetchSubWordAddr#
  , atomicExchangeWordAddr#
  , atomicExchangeAddrAddr#
  , atomicCasWord8Addr#
  , atomicCasWord16Addr#
  , atomicCasWord32Addr#
  , atomicCasWord64Addr#
  , atomicCasWordAddr#
  , atomicCasAddrAddr#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

#if SIMD
import Prelude hiding
  ( elem )

import qualified Prelude
  ( elem )
#endif

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
#if SIMD
  , pattern Vec2
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
  , pattern VecRep
#endif
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
#if SIMD
#if defined(x86_64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , Int8X16#
  , Int16X8#
  , Int32X4#
  , Int64X2#
  , Word8X16#
  , Word16X8#
  , Word32X4#
  , Word64X2#
  , FloatX4#
  , DoubleX2#
  , Int8X32#
  , Int16X16#
  , Int32X8#
  , Int64X4#
  , Word8X32#
  , Word16X16#
  , Word32X8#
  , Word64X4#
  , FloatX8#
  , DoubleX4#
  , Int8X64#
  , Int16X32#
  , Int32X16#
  , Int64X8#
  , Word8X64#
  , Word16X32#
  , Word32X16#
  , Word64X8#
  , FloatX16#
  , DoubleX8#
#else
  , Int8X16#
  , Int16X8#
  , Int32X4#
  , Int64X2#
  , Word8X16#
  , Word16X8#
  , Word32X4#
  , Word64X2#
  , FloatX4#
  , DoubleX2#
#endif
#elif defined(aarch64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , Int8X16#
  , Int16X8#
  , Int32X4#
  , Int64X2#
  , Word8X16#
  , Word16X8#
  , Word32X4#
  , Word64X2#
  , FloatX4#
  , DoubleX2#
#else
#endif
#endif
#endif
  , State#
#if SIMD
  , pattern I#
#endif
  , MutableByteArray#
  , ByteArray#
  )

import qualified GHC.Exts as GHC
  ( nullAddr#
  , eqAddr#
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
  , writeAddrOffAddr#
  , writeFloatOffAddr#
  , writeDoubleOffAddr#
#if SIMD
#if defined(x86_64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , writeInt8X16OffAddr#
  , writeInt16X8OffAddr#
  , writeInt32X4OffAddr#
  , writeInt64X2OffAddr#
  , writeWord8X16OffAddr#
  , writeWord16X8OffAddr#
  , writeWord32X4OffAddr#
  , writeWord64X2OffAddr#
  , writeFloatX4OffAddr#
  , writeDoubleX2OffAddr#
  , writeInt8X32OffAddr#
  , writeInt16X16OffAddr#
  , writeInt32X8OffAddr#
  , writeInt64X4OffAddr#
  , writeWord8X32OffAddr#
  , writeWord16X16OffAddr#
  , writeWord32X8OffAddr#
  , writeWord64X4OffAddr#
  , writeFloatX8OffAddr#
  , writeDoubleX4OffAddr#
  , writeInt8X64OffAddr#
  , writeInt16X32OffAddr#
  , writeInt32X16OffAddr#
  , writeInt64X8OffAddr#
  , writeWord8X64OffAddr#
  , writeWord16X32OffAddr#
  , writeWord32X16OffAddr#
  , writeWord64X8OffAddr#
  , writeFloatX16OffAddr#
  , writeDoubleX8OffAddr#
#else
  , writeInt8X16OffAddr#
  , writeInt16X8OffAddr#
  , writeInt32X4OffAddr#
  , writeInt64X2OffAddr#
  , writeWord8X16OffAddr#
  , writeWord16X8OffAddr#
  , writeWord32X4OffAddr#
  , writeWord64X2OffAddr#
  , writeFloatX4OffAddr#
  , writeDoubleX2OffAddr#
#endif
#elif defined(aarch64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , writeInt8X16OffAddr#
  , writeInt16X8OffAddr#
  , writeInt32X4OffAddr#
  , writeInt64X2OffAddr#
  , writeWord8X16OffAddr#
  , writeWord16X8OffAddr#
  , writeWord32X4OffAddr#
  , writeWord64X2OffAddr#
  , writeFloatX4OffAddr#
  , writeDoubleX2OffAddr#
#else
#endif
#endif
#endif
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
  , readAddrOffAddr#
  , readFloatOffAddr#
  , readDoubleOffAddr#  
#if SIMD
#if defined(x86_64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , readInt8X16OffAddr#
  , readInt16X8OffAddr#
  , readInt32X4OffAddr#
  , readInt64X2OffAddr#
  , readWord8X16OffAddr#
  , readWord16X8OffAddr#
  , readWord32X4OffAddr#
  , readWord64X2OffAddr#
  , readFloatX4OffAddr#
  , readDoubleX2OffAddr#
  , readInt8X32OffAddr#
  , readInt16X16OffAddr#
  , readInt32X8OffAddr#
  , readInt64X4OffAddr#
  , readWord8X32OffAddr#
  , readWord16X16OffAddr#
  , readWord32X8OffAddr#
  , readWord64X4OffAddr#
  , readFloatX8OffAddr#
  , readDoubleX4OffAddr#
  , readInt8X64OffAddr#
  , readInt16X32OffAddr#
  , readInt32X16OffAddr#
  , readInt64X8OffAddr#
  , readWord8X64OffAddr#
  , readWord16X32OffAddr#
  , readWord32X16OffAddr#
  , readWord64X8OffAddr#
  , readFloatX16OffAddr#
  , readDoubleX8OffAddr#
#else
  , readInt8X16OffAddr#
  , readInt16X8OffAddr#
  , readInt32X4OffAddr#
  , readInt64X2OffAddr#
  , readWord8X16OffAddr#
  , readWord16X8OffAddr#
  , readWord32X4OffAddr#
  , readWord64X2OffAddr#
  , readFloatX4OffAddr#
  , readDoubleX2OffAddr#
#endif
#elif defined(aarch64_HOST_ARCH)
#if defined(__GLASGOW_HASKELL_LLVM__)
  , readInt8X16OffAddr#
  , readInt16X8OffAddr#
  , readInt32X4OffAddr#
  , readInt64X2OffAddr#
  , readWord8X16OffAddr#
  , readWord16X8OffAddr#
  , readWord32X4OffAddr#
  , readWord64X2OffAddr#
  , readFloatX4OffAddr#
  , readDoubleX2OffAddr#
#else
#endif
#endif
#endif
  , readCharOffAddr#
  , readWideCharOffAddr#
  , copyByteArrayToAddr#
  , copyMutableByteArrayToAddr#
  , copyAddrToByteArray#
  , atomicWriteWordAddr#
  , atomicReadWordAddr#
  , fetchXorWordAddr#
  , fetchAndWordAddr#
  , fetchNandWordAddr#
  , fetchOrWordAddr#
  , fetchAddWordAddr#
  , fetchSubWordAddr#
  , atomicExchangeWordAddr#
  , atomicExchangeAddrAddr#
  , atomicCasWord8Addr#
  , atomicCasWord16Addr#
  , atomicCasWord32Addr#
  , atomicCasWord64Addr#
  , atomicCasWordAddr#
  , atomicCasAddrAddr#
  )

import Data.Coerce
  ( coerce )

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

import Data.Addr.TH
  ( Addr#
    ( Addr# )
  , Addrable
      ( writeAddr#
      , readAddr#
      )
  , deriveAddrable
  ) 


-- * Manual (i.e., non-GC, foreign heap) bytearray (a/rea/dea)llocation via 'Addr#'s

{- | Given argument @n@,
    returns the 'State#' action
    allocating at least @[p, p + n bytes)@
    and returning @p@\;
    wraps a @ccall@ to @malloc@
-}
foreign import prim "allocAddrBytesPrimOp"
    allocAddrBytes# ::
        forall s. Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @n@, @d@,
    returns the 'State#' action
    allocating at least @[p, p + n bytes)@ on the foreign heap
    with @p@ aligned to (a multiple of) @d@ bytes
    and returning @p@\;
    wraps a @ccall@ to @alloc_aligned@\;
    assumes that @n@ is a multiple of @d@
-}
foreign import prim "allocAddrBytesAlignedPrimOp"
    allocAddrBytesAligned# ::
        forall s. Int# -> Int# -> State# s -> (# State# s, Addr# s #)

{- | Given argument @n@,
    returns the 'State#' action
    allocating and clearing at least @[p, p + n bytes)@
    and returning @p@\;
    wraps a @ccall@ to @calloc@
-}
foreign import prim "callocAddrBytesPrimOp"
    callocAddrBytes# ::
        forall s. Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @n@, @d@,
    returns the 'State#' action
    allocating and clearing at least @[p, p + n bytes)@
    with @p@ aligned to (a multiple of) @d@ bytes    
    and returning @p@\;
    wraps a @ccall@ to @alloc_aligned@ and a @prim@ call to @memset@\;
    assumes that @n@ is a multiple of @d@
-}
foreign import prim "callocAddrBytesAlignedPrimOp"
    callocAddrBytesAligned# ::
        forall s. Int# -> Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    resizing @p@\'s allocation to @[q, q + n bytes)@ bytes
    and returning @q@\;
    wraps a @ccall@ to @realloc@
-}
foreign import prim "reallocAddrBytesPrimOp"
    reallocAddrBytes# ::
        forall s. Addr# s -> Int# -> State# s -> (# State# s, Addr# s #)

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
    setting @[p, p + n bytes)@ to @c@\;
    wraps a @prim@ call to @memset@
-}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesSigned# ::
        forall s. Addr# s -> Int# -> Int8# -> State# s -> State# s

{- | Given arguments @p@, @n@, @c@
    returns the 'State#' action
    setting @[p, p + n bytes)@ to @c@\;
    wraps a @prim@ call to @memset@
-}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesUnsigned# ::
        forall s. Addr# s -> Int# -> Word8# -> State# s -> State# s

{- | Given arguments @p_src@, @p_dst@, @n@,
    where @[p_src, p_src + n bytes)@ may overlap @[p_dst, p_dst + n bytes)@,
    returns the 'State#' action
    copying @[p_src, p_src + n bytes)@ to @[p_dst, p_dst + n bytes)@\;
    wraps a @prim@ call to @memmove@
-}
foreign import prim "copyAddrBytesPrimOp"
    copyAddrBytes# ::
        forall s. Addr# s -> Addr# s -> Int# -> State# s -> State# s

{- | Given arguments @p_src@, @p_dst@, @n@,
    where @[p_src, p_src + n bytes)@ and @[p_dst, p_dst + n bytes)@ are assumed to not overlap,
    returns the 'State#' action
    copying @[p_src, p_src + n bytes)@ to @[p_dst, p_dst + n bytes)@\;
    wraps a @prim@ call to @memcpy@
-}
foreign import prim "copyAddrNonOverlappingBytesPrimOp"
    copyAddrNonOverlappingBytes# ::
        forall s. Addr# s -> Addr# s -> Int# -> State# s -> State# s


-- * 'Addr#' arithmetic

-- _ Thanks to Jaror for this idea!
{- | The null address -}
pattern NullAddr# :: forall s. Addr# s
pattern NullAddr# <- ((\ (Addr# a) -> GHC.eqAddr# GHC.nullAddr# a) -> 1#) where
    NullAddr# = Addr# GHC.nullAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is equal to @p1@ and @0#@ otherwise
-}
{-# INLINE eqAddr# #-}
eqAddr# :: forall s. Addr# s -> Addr# s -> Int#
eqAddr# = coerce GHC.eqAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is not equal to @p1@ and @0#@ otherwise
-}
{-# INLINE neAddr# #-}
neAddr# :: forall s. Addr# s -> Addr# s -> Int#
neAddr# = coerce GHC.neAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE geAddr# #-}
geAddr# :: forall s. Addr# s -> Addr# s -> Int#
geAddr# = coerce GHC.geAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than @p1@ and @0#@ otherwise
-}
{-# INLINE gtAddr# #-}
gtAddr# :: forall s. Addr# s -> Addr# s -> Int#
gtAddr# = coerce GHC.gtAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE leAddr# #-}
leAddr# :: forall s. Addr# s -> Addr# s -> Int#
leAddr# = coerce GHC.leAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than @p1@ and @0#@ otherwise
-}
{-# INLINE ltAddr# #-}
ltAddr# :: forall s. Addr# s -> Addr# s -> Int#
ltAddr# = coerce GHC.ltAddr#

{- | Given arguments @p@, @n@,
    returns the machine address an offset of @n@ bytes from @p@
-}
{-# INLINE plusAddrBytes# #-}
plusAddrBytes# :: forall s. Addr# s -> Int# -> Addr# s
plusAddrBytes# = coerce GHC.plusAddr#

{- | Given arguments @p0@, @p1@,
    returns the offset of @p0@ from @p1@ in bytes
-}
{-# INLINE minusAddrBytes# #-}
minusAddrBytes# :: forall s. Addr# s -> Addr# s -> Int#
minusAddrBytes# = coerce GHC.minusAddr#

{- | Given arguments @p@, @m@,
    returns the remainder in bytes when @p@ is divided by @m@
-}
{-# INLINE remAddrBytes# #-}
remAddrBytes# :: forall s. Addr# s -> Int# -> Int#
remAddrBytes# = coerce GHC.remAddr#


-- * Prefetching via 'Addr#'s

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    prefetching @p + n bytes@ to a register
-}
{-# INLINE prefetchAddr0# #-}
prefetchAddr0# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr0# = coerce GHC.prefetchAddr0#

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    prefetching @p + n bytes@ to the L1 cache
-}
{-# INLINE prefetchAddr1# #-}
prefetchAddr1# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr1# = coerce GHC.prefetchAddr1#

{- | Given argument @p@, @n@,
    returns the 'State#' action
    prefetching @p + n bytes@ to the L2 cache
-}
{-# INLINE prefetchAddr2# #-}
prefetchAddr2# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr2# = coerce GHC.prefetchAddr2#

{- | Given argument @p@, @n@,
    returns the 'State#' action
    prefetching @p + n bytes@ to the L3 cache
-}
{-# INLINE prefetchAddr3# #-}
prefetchAddr3# :: forall s. Addr# s -> Int# -> State# s -> State# s
prefetchAddr3# = coerce GHC.prefetchAddr3#


-- * Writing/reading off 'Addr#'s

{- | Instantiates 'Addrable' for various 'RuntimeRep's -}
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
                      , FloatRep
                      , DoubleRep ]
                    [ a ]
                Lim  -> [ ]
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
                    let v = VecRep c e
                    case Prelude.elem (I# (repBytes v)) supportedSIMDBytes of
                        True  -> [ v ]
                        False -> [ ]
#else
                Vec  -> [ ]
#endif
                Box  -> [ ]
            [ r ]
    r <- sr
    [ deriveAddrable r ]
  )

{- | Given arguments @p@, @n@, @c@,
    where @c@ is assumed to be @1@ byte,
    returns the 'State#' action
    writing @c@ to @p + n bytes@
-}
{-# INLINE writeCharOffAddr# #-}
writeCharOffAddr# ::
    forall s. Addr# s -> Int# -> Char# -> State# s -> State# s
writeCharOffAddr# = coerce GHC.writeCharOffAddr#

{- | Given arguments @p@, @n@, @c@,
    where @c@ is assumed to be @4@ bytes,
    returns the 'State#' action
    writing @c + 4 * n bytes@ 
-}
{-# INLINE writeWideCharOffAddr# #-}
writeWideCharOffAddr# ::
    forall s. Addr# s -> Int# -> Char# -> State# s -> State# s
writeWideCharOffAddr# = coerce GHC.writeWideCharOffAddr#

{- | Given arguments @p@, @n@, @q@,
    returns the 'State#' action
    writing @q@ to @p + repBytes(AddrRep) * n bytes@
-}
{-# INLINE writeAddrOffAddr# #-}
writeAddrOffAddr# ::
    forall s. Addr# s -> Int# -> Addr# s -> State# s -> State# s
writeAddrOffAddr# = coerce GHC.writeAddrOffAddr#

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading @c@ from @p + n bytes@
    where @c@ is assumed to be @1@ byte,
    and returning @c@
-}
{-# INLINE readCharOffAddr# #-}
readCharOffAddr# ::
    forall s. Addr# s -> Int# -> State# s -> (# State# s, Char# #)
readCharOffAddr# = coerce GHC.readCharOffAddr#

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading @c@ from @p + 4 * n bytes@
    where @c@ is assumed to be @4@ bytes,
    and returning @c@
-}
{-# INLINE readWideCharOffAddr# #-}
readWideCharOffAddr# ::
    forall s. Addr# s -> Int# -> State# s -> (# State# s, Char# #)
readWideCharOffAddr# = coerce GHC.readWideCharOffAddr#

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading @q@ from @p + n * repBytes(AddrRep) bytes@
    and returning @q@
-}
{-# INLINE readAddrOffAddr# #-}
readAddrOffAddr# ::
    forall s. Addr# s -> Int# -> State# s -> (# State# s, Addr# s #)
readAddrOffAddr# = coerce GHC.readAddrOffAddr#


-- * Interoperation with GHC's 'ByteArray#'/'MutableByteArray#'

{- | Given arguments @w@, @i@, @p@, @n@,
    returns the 'State#' action
    copying @[w + i bytes, w + i bytes + n bytes)@ to @[p, p + n bytes)@
-}
{-# INLINE copyMutableByteArrayToAddr# #-}
copyMutableByteArrayToAddr# ::
    forall s. MutableByteArray# s -> Int# -> Addr# s -> Int# -> State# s -> State# s
copyMutableByteArrayToAddr# = coerce GHC.copyMutableByteArrayToAddr#

{- | Given arguments @v@, @i@, @p@, @n@,
    returns the 'State#' action
    copying @[v + i bytes, v + i bytes + n bytes)@ to @[p, p + n bytes)@
-}
{-# INLINE copyByteArrayToAddr# #-}
copyByteArrayToAddr# ::
    forall s. ByteArray# -> Int# -> Addr# s -> Int# -> State# s -> State# s
copyByteArrayToAddr# = coerce GHC.copyByteArrayToAddr#

{- | Given arguments @p@, @w@, @i@, @n@,
    returns the 'State#' action
    copying @[p, p + n bytes)@ to @[w + i bytes, w + i bytes + n bytes)@
-}
{-# INLINE copyAddrToMutableByteArray# #-}
copyAddrToMutableByteArray# ::
    forall s. Addr# s -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyAddrToMutableByteArray# = coerce GHC.copyAddrToByteArray#


-- * Concurrency primitives

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    writing @n@ to @p@\;
    implies a full memory barrier
-}
{-# INLINE atomicWriteWordAddr# #-}
atomicWriteWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> State# s
atomicWriteWordAddr# = coerce GHC.atomicWriteWordAddr#

{- | Given argument @p@,
    returns the atomic 'State#' action
    reading @n@ off @p@
    and returning @n@\;
    implies a full memory barrier
-}
{-# INLINE atomicReadWordAddr# #-}
atomicReadWordAddr# ::
    forall s. Addr# s -> State# s -> (# State# s, Word# #)
atomicReadWordAddr# = coerce GHC.atomicReadWordAddr#

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    reading @n'@ off @p@,
    computing @n'' := n XOR n'@,
    writing @n''@ to @p@,
    and returning @n'@\;
    implies a full memory barrier
-}
{-# INLINE fetchXorWordAddr# #-}
fetchXorWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> (# State# s, Word# #)
fetchXorWordAddr# = coerce GHC.fetchXorWordAddr#

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    reading @n'@ off @p@,
    computing @n'' := n AND n'@,
    writing @n''@ to @p@,
    and returning @n'@\;
    implies a full memory barrier
-}
{-# INLINE fetchAndWordAddr# #-}
fetchAndWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> (# State# s, Word# #)
fetchAndWordAddr# = coerce GHC.fetchAndWordAddr#

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    reading @n'@ off @p@,
    computing @n'' := n NAND n'@,
    writing @n''@ to @p@,
    and returning @n'@\;
    implies a full memory barrier
-}
{-# INLINE fetchNandWordAddr# #-}
fetchNandWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> (# State# s, Word# #)
fetchNandWordAddr# = coerce GHC.fetchNandWordAddr#

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    reading @n'@ off @p@,
    computing @n'' := n OR n'@,
    writing @n''@ to @p@,
    and returning @n'@\;
    implies a full memory barrier
-}
{-# INLINE fetchOrWordAddr# #-}
fetchOrWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> (# State# s, Word# #)
fetchOrWordAddr# = coerce GHC.fetchOrWordAddr#

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    reading @n'@ off @p@,
    computing @n'' := n + n'@,
    writing @n''@ to @p@,
    and returning @n'@\;
    implies a full memory barrier
-}
{-# INLINE fetchAddWordAddr# #-}
fetchAddWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> (# State# s, Word# #)
fetchAddWordAddr# = coerce GHC.fetchAddWordAddr#

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    reading @n'@ off @p@,
    computing @n'' := n' - n@,
    writing @n''@ to @p@,
    and returning @n'@\;
    implies a full memory barrier
-}
{-# INLINE fetchSubWordAddr# #-}
fetchSubWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> (# State# s, Word# #)
fetchSubWordAddr# = coerce GHC.fetchSubWordAddr#

{- | Given arguments @p@, @n@,
    returns the atomic 'State#' action
    reading @n'@ off @p@,
    writing @n@ to @p@,
    and returning @n'@\;
    implies a read barrier
-}
{-# INLINE atomicExchangeWordAddr# #-}
atomicExchangeWordAddr# ::
    forall s. Addr# s -> Word# -> State# s -> (# State# s, Word# #)
atomicExchangeWordAddr# = coerce GHC.atomicExchangeWordAddr#

{- | Given arguments @p@, @q@,
    returns the atomic 'State#' action
    reading @q'@ off @p@,
    writing @q@ to @p@,
    and returning @q'@\;
    implies a read barrier
-}
{-# INLINE atomicExchangeAddrAddr# #-}
atomicExchangeAddrAddr# ::
    forall s. Addr# s -> Addr# s -> State# s -> (# State# s, Addr# s #)
atomicExchangeAddrAddr# = coerce GHC.atomicExchangeAddrAddr#

{- | Given arguments @p@, @n0@, @n1@,
    returns the atomic 'State#' action
    reading @n@ off @p@,
    writing @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    and returning @n@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord8Addr# #-}
atomicCasWord8Addr# ::
    forall s. Addr# s -> Word8# -> Word8# -> State# s -> (# State# s, Word8# #)
atomicCasWord8Addr# = coerce GHC.atomicCasWord8Addr#

{- | Given arguments @p@, @n0@, @n1@,
    returns the atomic 'State#' action
    reading @n@ off @p@,
    writing @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    and returning @n@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord16Addr# #-}
atomicCasWord16Addr# ::
    forall s. Addr# s -> Word16# -> Word16# -> State# s -> (# State# s, Word16# #)
atomicCasWord16Addr# = coerce GHC.atomicCasWord16Addr#

{- | Given arguments @p@, @n0@, @n1@,
    returns the atomic 'State#' action
    reading @n@ off @p@,
    writing @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    and returning @n@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord32Addr# #-}
atomicCasWord32Addr# ::
    forall s. Addr# s -> Word32# -> Word32# -> State# s -> (# State# s, Word32# #)
atomicCasWord32Addr# = coerce GHC.atomicCasWord32Addr#

{- | Given arguments @p@, @n0@, @n1@,
    returns the atomic 'State#' action
    reading @n@ off @p@,
    writing @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    and returning @n@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord64Addr# #-}
atomicCasWord64Addr# ::
    forall s. Addr# s -> Word64# -> Word64# -> State# s -> (# State# s, Word64# #)
atomicCasWord64Addr# = coerce GHC.atomicCasWord64Addr#

{- | Given arguments @p@, @n0@, @n1@,
    returns the atomic 'State#' action
    reading @n@ off @p@,
    writing @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    and returning @n@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWordAddr# #-}
atomicCasWordAddr# ::
    forall s. Addr# s -> Word# -> Word# -> State# s -> (# State# s, Word# #)
atomicCasWordAddr# = coerce GHC.atomicCasWordAddr#

{- | Given arguments @p@, @q0@, @q1@,
    returns the atomic 'State#' action
    reading @q@ off @p@,
    writing @q1@ to @p@ iff @q0@ agrees with @q@
    (doing nothing otherwise),
    and returning @q@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasAddrAddr# #-}
atomicCasAddrAddr# ::
    forall s. Addr# s -> Addr# s -> Addr# s -> State# s -> (# State# s, Addr# s #)
atomicCasAddrAddr# = coerce GHC.atomicCasAddrAddr#