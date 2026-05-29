{-# LANGUAGE Haskell2010
  , CPP
  , DataKinds
  , FlexibleInstances
  , GHCForeignImportPrim
  , InstanceSigs
  , KindSignatures
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-| @-Woverlapping-patterns@ and @Winaccessible-code@ are disabled
    as they only fire due to the match on 'UnsafeRefl'.
    @-Worphans@ is disabled so that we can
    generate 'Addrable' instances (defined in "Data.Addr.Linear.TH")
    in this module ("Data.Addr.Linear") for types defined in "GHC.Exts"\;
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

  * Resolve issue #18472, allowing the below FFI imports to be greatly simplified

  * Upgrade GHC's SIMD support (cf. issue #25030)

  * Case SIMD support on more host archs (cf. "GHC.Platform.ArchOS")
-}

{- | @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses -}
module Data.Addr.Linear
  ( -- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses
    Addr#
    -- * Linear (i.e., non-GC, foreign heap) bytearray (a/rea/dea)llocation via 'Addr#'s
  , allocAddrBytes#
  , allocAddrBytesAligned#
  , callocAddrBytes#
  , callocAddrBytesAligned#
  , reallocAddrBytes#
  , freeAddr#
    -- * Machine 'Addr#' arithmetic
  , isNullAddr#
  , eqAddr#
  , neAddr#
  , geAddr#
  , gtAddr#
  , leAddr#
  , ltAddr#
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
  , atomicCasWord8Addr#
  , atomicCasWord16Addr#
  , atomicCasWord32Addr#
  , atomicCasWord64Addr#
  , atomicCasWordAddr#
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
  , pattern One
  , pattern Many
  , (*#)
#if SIMD
  , pattern I#
#endif
  , MutableByteArray#
  , ByteArray#
  )

import qualified GHC.Exts
  ( writeInt8OffAddr#
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
  )

import qualified GHC.Exts as GHC
  ( Addr#
  , nullAddr#
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
  , atomicCasWord8Addr#
  , atomicCasWord16Addr#
  , atomicCasWord32Addr#
  , atomicCasWord64Addr#
  , atomicCasWordAddr#
  )

import Data.Coerce
  ( coerce )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ (internal)

import Data.RuntimeRep
  ( pattern Prim
  , pattern Lim
  , pattern Vec
  , pattern Box
  , repBytes
#if SIMD
  , supportedSIMDBytes
#endif
  )

import Prelude.Linear
  ( Ur
  , ur
  )

import Data.State
  ( refresh# )

import Data.State.Linear
  ( Alloc# )

import Data.State.Linear.Unsafe
  ( pattern Alloc# )

import Data.Addr.Linear.TH
  ( Addr#
      ( Addr# )
  , Addrable
      ( writeAddr#
      , readAddr#
      )
  , declareAddrableEg
  )


-- * @'TYPE' ('BoxedRep' 'Lifted')@-parametrized machine addresses

-- * Manual (i.e., non-GC, foreign heap) bytearray (a/rea/dea)llocation via 'Addr#'s

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "allocAddrBytesPrimOp"
    allocAddrBytes_primOp# ::
        forall t. Int# %Many-> Alloc# t %Many-> Addr# t
{- | Given arguments @n@, @t@,
    allocates at least @[p, p + n bytes)@ on the foreign heap,
    binds @t@,
    and returns @p@\;
    wraps a @ccall@ to @malloc@
-}
{-# INLINE allocAddrBytes# #-}
allocAddrBytes# ::
    forall t. Int# %One-> Alloc# t %One-> Addr# t
allocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> allocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "allocAddrBytesAlignedPrimOp"
    allocAddrBytesAligned_primOp# ::
        forall t. Int# %Many-> Int# %Many-> Alloc# t %Many-> Addr# t
{- | Given arguments @n@, @d@, @t@,
    allocates at least @[p, p + n bytes)@ on the foreign heap
    with @p@ aligned to (a multiple of) @d@ bytes,
    binds @t@,
    rand returns @p@\;
    wraps a @ccall@ to @alloc_aligned@\;
    assumes that @n@ is a multiple of @d@
-}
{-# INLINE allocAddrBytesAligned# #-}
allocAddrBytesAligned# ::
    forall t. Int# %One-> Int# %One-> Alloc# t %One-> Addr# t
allocAddrBytesAligned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> allocAddrBytesAligned_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "callocAddrBytesPrimOp"
    callocAddrBytes_primOp# ::
        forall t. Int# %Many-> Alloc# t %Many-> Addr# t
{- | Given arguments @n@, @t@,
    allocates and clears at least @[p, p + n bytes)@ on the foreign heap,
    binds @t@,
    and returns @p@\;
    wraps a @ccall@ to @calloc@
-}
{-# INLINE callocAddrBytes# #-}
callocAddrBytes# ::
    forall t. Int# %One-> Alloc# t %One-> Addr# t
callocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> callocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "callocAddrBytesAlignedPrimOp"
    callocAddrBytesAligned_primOp# ::
        forall t. Int# %Many-> Int# %Many-> Alloc# t %Many-> Addr# t
{- | Given arguments @n@, @d@, @t@,
    allocates and clears at least @[p, p + n bytes)@ on the foreign heap
    with @p@ aligned to (a multiple of) @d@ bytes,   
    binds @t@,
    and returns @p@\;
    wraps a @ccall@ to @alloc_aligned@ and a @prim@ call to @memset@\;
    assumes that @n@ is a multiple of @d@
-}
{-# INLINE callocAddrBytesAligned# #-}
callocAddrBytesAligned# ::
    forall t. Int# %One-> Int# %One-> Alloc# t %One-> Addr# t
callocAddrBytesAligned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> callocAddrBytesAligned_primOp#

{- _ Cf. @GHC-57396@ and @GHC-43510@ as to why this song and dance is necessary -}
foreign import prim "reallocAddrBytesPrimOp"
    reallocAddrBytes_primOp# ::
        forall t. GHC.Addr# %Many-> Int# %Many-> State# t %Many-> Addr# t
{- | Given arguments @p@, @n@,
    resizes @p@\'s allocation to at least @[q, q + n bytes)@
    and returns @q@\;
    wraps a @ccall@ to @realloc@
-}
{-# INLINE reallocAddrBytes# #-}
reallocAddrBytes# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
reallocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) n ->
        reallocAddrBytes_primOp# a n s

{- _ Cf. @GHC-57396@ and @GHC-43510@ as to why this song and dance is necessary -}
foreign import prim "freeAddrPrimOp"
    freeAddr_primOp# ::
        forall t. GHC.Addr# %Many-> State# t %Many-> Alloc# t
{- | Given argument @p@,
    frees @p@\'s allocation,
    unbinds an allocation token @t@,
    and returns @t@\;
    wraps a @ccall@ to @free@
-}
{-# INLINE freeAddr# #-}
freeAddr# ::
    forall t. Addr# t %One-> Alloc# t
freeAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) ->
        freeAddr_primOp# a s


-- * Basic bulk byte manipulations

{- _ Cf. @GHC-43510@ as to why this song and dance is necessary -}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesSigned_primOp# ::
        forall t. GHC.Addr# %Many-> Int# %Many-> Int8# %Many->
        State# t %Many-> State# t
{- | Given arguments @p@, @n@, @c@,
    sets @[p, p + n bytes)@ to @c@
    and returns @p@\;
    wraps a @prim@ call to @memset@
-}
{-# INLINE setAddrBytesSigned# #-}
setAddrBytesSigned# ::
    forall t. Addr# t %One-> Int# %One-> Int8# %One-> Addr# t
setAddrBytesSigned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) n c ->
        case setAddrBytesSigned_primOp# a n c s of
            s' -> (# s', a #)

{- _ Cf. @GHC-43510@ as to why this song and dance is necessary -}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesUnsigned_primOp# ::
        forall t. GHC.Addr# %Many-> Int# %Many-> Word8# %Many->
        State# t %Many-> State# t
{- | Given arguments @p@, @n@, @c@,
    sets @[p, p + n bytes)@ to @c@
    and returns @p@\;
    wraps a @prim@ call to @memset@
-}
{-# INLINE setAddrBytesUnsigned# #-}
setAddrBytesUnsigned# ::
    forall t. Addr# t %One-> Int# %One-> Word8# %One-> Addr# t
setAddrBytesUnsigned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) n c ->
        case setAddrBytesUnsigned_primOp# a n c s of
            s' -> (# s', a #)

{- _ Cf. @GHC-43510@ as to why this song and dance is necessary -}
foreign import prim "copyAddrBytesPrimOp"
    copyAddrBytes_primOp# ::
        forall t. GHC.Addr# %Many-> GHC.Addr# %Many-> Int# %Many->
        State# t %Many-> State# t %Many-> State# t
{- | Given arguments @p_src@, @p_dst@, @n@,
    where @[p_src, p_src + n bytes)@ and @[p_dst, p_dst + n bytes)@ may overlap,
    copies @[p_src, p_src + n bytes)@ to @[p_dst, p_dst + n bytes)@
    and returns @p_src@, @p_dst@\;
    wraps a @prim@ call to @memmove@
-}
{-# INLINE copyAddrBytes# #-}
copyAddrBytes# ::
    forall t. Addr# t %One-> Addr# t %One-> Int# %One-> (# Addr# t, Addr# t #)
copyAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s_src, a_src #) (# s_dst, a_dst #) n ->
        case copyAddrBytes_primOp# a_src a_dst n s_src s_dst of
            s' -> (# (# s', a_src #), (# s', a_dst #) #)

{- _ Cf. @GHC-43510@ as to why this song and dance is necessary -}
foreign import prim "copyAddrNonOverlappingBytesPrimOp"
    copyAddrNonOverlappingBytes_primOp# ::
        forall t. GHC.Addr# %Many-> GHC.Addr# %Many-> Int# %Many->
        State# t %Many-> State# t %Many-> State# t
{- | Given arguments @p_src@, @p_dst@, @n@,
    where @[p_src, p_src + n bytes)@ and @[p_dst, p_dst + n bytes)@ are assumed to not overlap,
    copies @[p_src, p_src + n bytes)@ to @[p_dst, p_dst + n bytes)@
    and returns @p_src@, @p_dst@\;
    wraps a @prim@ call to @memcpy@
-}
{-# INLINE copyAddrNonOverlappingBytes# #-}
copyAddrNonOverlappingBytes# ::
    forall t. Addr# t %One-> Addr# t %One-> Int# %One-> (# Addr# t, Addr# t #)
copyAddrNonOverlappingBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s_src, a_src #) (# s_dst, a_dst #) n ->
        case copyAddrNonOverlappingBytes_primOp# a_src a_dst n s_src s_dst of
            s' -> (# (# s', a_src #), (# s', a_dst #) #)


-- * 'Addr#' arithmetic

{- | Given argument @p@,
    returns @p@
    and non-@0#@ iff @p@ is equal to the null address
-}
isNullAddr# :: forall t. Addr# t %One-> (# Addr# t, Int# #)
isNullAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p@(# _, a #) ->
        (# p, GHC.eqAddr# GHC.nullAddr# a #)

{- | Given arguments @p0@, @p1@,
    returns @p0@, @p1@,
    and non-@0#@ iff @p0@ is equal to @p1@
-}
{-# INLINE eqAddr# #-}
eqAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> (# Addr# t, Addr# t, Int# #)
eqAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p0@(# _, a0 #) p1@(# _, a1 #) ->
        (# p0, p1, GHC.eqAddr# a0 a1 #)

{- | Given arguments @p0@, @p1@,
    returns @p0@, @p1@,
    and non-@0#@ iff @p0@ is not equal to @p1@
-}
{-# INLINE neAddr# #-}
neAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> (# Addr# t, Addr# t, Int# #)
neAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p0@(# _, a0 #) p1@(# _, a1 #) ->
        (# p0, p1, GHC.neAddr# a0 a1 #)

{- | Given arguments @p0@, @p1@,
    returns @p0@, @p1@,
    and non-@0#@ iff @p0@ is greater than or equal to @p1@
-}
{-# INLINE geAddr# #-}
geAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> (# Addr# t, Addr# t, Int# #)
geAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p0@(# _, a0 #) p1@(# _, a1 #) ->
        (# p0, p1, GHC.geAddr# a0 a1 #)

{- | Given arguments @p0@, @p1@,
    returns @p0@, @p1@,
    and non-@0#@ iff @p0@ is greater than @p1@
-}
{-# INLINE gtAddr# #-}
gtAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> (# Addr# t, Addr# t, Int# #)
gtAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p0@(# _, a0 #) p1@(# _, a1 #) ->
        (# p0, p1, GHC.gtAddr# a0 a1 #)

{- | Given arguments @p0@, @p1@,
    returns @p0@, @p1@,
    and non-@0#@ iff @p0@ is less than or equal to @p1@
-}
{-# INLINE leAddr# #-}
leAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> (# Addr# t, Addr# t, Int# #)
leAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p0@(# _, a0 #) p1@(# _, a1 #) ->
        (# p0, p1, GHC.leAddr# a0 a1 #)

{- | Given arguments @p0@, @p1@,
    returns @p0@, @p1@,
    and non-@0#@ iff @p0@ is less than @p1@
-}
{-# INLINE ltAddr# #-}
ltAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> (# Addr# t, Addr# t, Int# #)
ltAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p0@(# _, a0 #) p1@(# _, a1 #) ->
        (# p0, p1, GHC.ltAddr# a0 a1 #)

{- | Given arguments @p0@, @p1@,
    returns @p0@, @p1@,
    and the offset @p0 - p1@ in bytes
-}
{-# INLINE minusAddrBytes# #-}
minusAddrBytes# ::
    forall t. Addr# t %One-> Addr# t %One-> (# Addr# t, Addr# t, Int# #)
minusAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p0@(# _, a0 #) p1@(# _, a1 #) ->
        (# p0, p1, GHC.minusAddr# a0 a1 #)

{- | Given arguments @p@, @d@,
    returns @p@
    and the remainder in bytes when @p@ is divided by @d@
-}
{-# INLINE remAddrBytes# #-}
remAddrBytes# ::
    forall t. Addr# t %One-> Int# %One-> (# Addr# t, Int# #)
remAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ p@(# _, a #) d ->
        (# p, GHC.remAddr# a d #)


-- * Prefetching via 'Addr#'s

{- | Given arguments @p@, @i@,
    prefetches @p + i bytes@ to a register
    and returns @p@
-}
{-# INLINE prefetchAddr0# #-}
prefetchAddr0# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr0# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i ->
        case GHC.prefetchAddr0# a i s of
            s' -> (# s', a #)

{- | Given arguments @p@, @i@,
    prefetches @p + i bytes@ to the L1 cache
    and returns @p@
-}
{-# INLINE prefetchAddr1# #-}
prefetchAddr1# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr1# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i ->
        case GHC.prefetchAddr1# a i s of
            s' -> (# s', a #)

{- | Given arguments @p@, @i@,
    prefetches @p + i bytes@ to the L2 cache
    and returns @p@
-}
{-# INLINE prefetchAddr2# #-}
prefetchAddr2# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr2# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i ->
        case GHC.prefetchAddr2# a i s of
            s' -> (# s', a #)

{- | Given arguments @p@, @i@,
    prefetches @p + i bytes@ to the L3 cache
    and returns @p@
-}
{-# INLINE prefetchAddr3# #-}
prefetchAddr3# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr3# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i ->
        case GHC.prefetchAddr3# a i s of
            s' -> (# s', a #)


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
    [ declareAddrableEg r ]
  )

{- | Given arguments @p@, @i@, @c@,
    where @c@ is assumed to be @1@ byte,
    writes @c@ to @p + i bytes@
    and returns @p@
-}
{-# INLINE writeCharOffAddr# #-}
writeCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> Char# %One-> Addr# t
writeCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i c ->
        case GHC.Exts.writeCharOffAddr# a i c s of
            s' -> (# s', a #)

{- | Given arguments @p@, @i@, @c@,
    where @c@ is assumed to be @4@ bytes,
    writes @c@ to @p + 4 * i bytes@
    and returns @p@
-}
{-# INLINE writeWideCharOffAddr# #-}
writeWideCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> Char# %One-> Addr# t
writeWideCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i c ->
        case GHC.Exts.writeWideCharOffAddr# a i c s of
            s' -> (# s', a #)

{- | Given arguments @p@, @i@,
    reads @c@ from @p + i bytes@
    where @c@ is assumed to be @1@ byte,
    and returns @p@, @c@
-}
{-# INLINE readCharOffAddr# #-}
readCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> (# Addr# t, Ur Char# #)
readCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i ->
        case GHC.Exts.readCharOffAddr# a i s of
            (# s', c #) -> (# (# s', a #), ur c #)

{- | Given arguments @p@, @i@,
    reads @c@ from @p + 4 * i bytes@
    where @c@ is assumed to be @4@ bytes,
    and returns @p@, @c@
-}
{-# INLINE readWideCharOffAddr# #-}
readWideCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> (# Addr# t, Ur Char# #)
readWideCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i ->
        case GHC.Exts.readWideCharOffAddr# a i s of
            (# s', c #) -> (# (# s', a #), ur c #)


-- * Interoperation with GHC's 'ByteArray#'/'MutableByteArray#'

{- | Given arguments @w@, @j@, @p@, @i@, @n@,
    returns the linear 'State#' action
    copying @[w + j bytes, w + j bytes + n bytes)@ to @[p + i bytes, p + i bytes + n bytes)@
    and returning @p@
-}
{-# INLINE copyMutableByteArrayToAddr# #-}
copyMutableByteArrayToAddr# ::
    forall t s.
    MutableByteArray# s %One-> Int# %One-> Addr# t %One-> Int# %One-> Int# %One->
    State# s %One-> (# State# s, Addr# t #)
copyMutableByteArrayToAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ w j (# t, a #) i n s ->
        case (# GHC.copyMutableByteArrayToAddr# w j (GHC.plusAddr# a i) n s, refresh# t #) of
            (# s', t' #) -> (# s', (# t', a #) #)

{- | Given arguments @v@, @j@, @p@, @i@, @n@,
    copies @[v + j bytes, w + j bytes + n bytes)@ to @[p + i bytes, p + i bytes + n bytes)@
    and returns @p@
-}
{-# INLINE copyByteArrayToAddr# #-}
copyByteArrayToAddr# ::
    forall t.
    ByteArray# %One-> Int# %One-> Addr# t %One-> Int# %One-> Int# %One->
    Addr# t
copyByteArrayToAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ v j (# s, a #) i n ->
        case GHC.copyByteArrayToAddr# v j (GHC.plusAddr# a i) n s of
            s' -> (# s', a #)

{- | Given arguments @p@, @i@, @w@, @j@, @n@,
    returns the linear 'State#' action
    copying @[p + i bytes, p + i bytes + n bytes)@ to @[w + j bytes, w + j bytes + n bytes)@
    and returning @p@
-}
{-# INLINE copyAddrToMutableByteArray# #-}
copyAddrToMutableByteArray# ::
    forall t s.
    Addr# t %One-> Int# %One-> MutableByteArray# s %One-> Int# %One-> Int# %One->
    State# s %One-> (# State# s, Addr# t #)
copyAddrToMutableByteArray# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# t, a #) i w j n s ->
        case (# GHC.copyAddrToByteArray# (GHC.plusAddr# a i) w j n s, refresh# t #) of
            (# s', t' #) -> (# s', (# t', a #) #)


-- * Concurrency primitives

{- | Given arguments @p@, @u@, @i@
    atomically
    writes @u@ to @p + repBytes WordRep * i bytes@
    and returns @p@\;
    implies a full memory barrier
-}
{-# INLINE atomicWriteWordAddr# #-}
atomicWriteWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> Addr# t
atomicWriteWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.atomicWriteWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            s' -> (# s', a #)


{- | Given arguments @p@, @i@,
    atomicallt
    reads @u@ off @p + repBytes WordRep * i bytes@
    and returns @p@, @u@\;
    implies a full memory barrier
-}
{-# INLINE atomicReadWordAddr# #-}
atomicReadWordAddr# ::
    forall t. Addr# t %One-> Int# %One-> (# Addr# t, Word# #)
atomicReadWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) i ->
        case GHC.atomicReadWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) s of
            (# s', u #) -> (# (# s', a #), u #)

{- | Given arguments @p@, @u@, @i@,
    atomically
    reads @u'@ off @p + repBytes WordRep * i bytes@,
    computes @u'' := u XOR u'@,
    writes @u''@ to @p@,
    and returns @p@, @u'@\;
    implies a full memory barrier
-}
{-# INLINE fetchXorWordAddr# #-}
fetchXorWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
fetchXorWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.fetchXorWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u@, @i@,
    atomically
    reads @u'@ off @p + repBytes WordRep * i bytes@,
    computes @u'' := u AND u'@,
    writes @u''@ to @p@,
    and returns @p@, @u'@\;
    implies a full memory barrier
-}
{-# INLINE fetchAndWordAddr# #-}
fetchAndWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
fetchAndWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.fetchAndWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u@, @i@,
    atomically
    reads @u'@ off @p + repBytes WordRep * i bytes@,
    computes @u'' := u NAND u'@,
    writes @u''@ to @p@,
    and returns @p@, @u'@\;
    implies a full memory barrier
-}
{-# INLINE fetchNandWordAddr# #-}
fetchNandWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
fetchNandWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.fetchNandWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u@, @i@,
    atomically
    reads @u'@ off @p + repBytes WordRep * i bytes@,
    computes @u'' := u OR u'@,
    writes @u''@ to @p@,
    and returns @p@, @u'@\;
    implies a full memory barrier
-}
{-# INLINE fetchOrWordAddr# #-}
fetchOrWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
fetchOrWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.fetchOrWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u@, @i@,
    atomically
    reads @u'@ off @p + repBytes WordRep * i bytes@,
    computes @u'' := u + u'@,
    writes @u''@ to @p@,
    and returns @p@, @u'@\;
    implies a full memory barrier
-}
{-# INLINE fetchAddWordAddr# #-}
fetchAddWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
fetchAddWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.fetchAddWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u@, @i@,
    atomically
    reads @u'@ off @p + repBytes WordRep * i bytes@,
    computes @u'' := u' - u@,
    writes @u''@ to @p@,
    and returns @p@, @u'@\;
    implies a full memory barrier
-}
{-# INLINE fetchSubWordAddr# #-}
fetchSubWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
fetchSubWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.fetchSubWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            (# s', u' #) -> (# (# s', a #), u' #)


{- | Given arguments @p@, @u@, @i@,
    atomically
    reads @u'@ off @p + repBytes WordRep * i bytes@,
    writes @u@ to @p@,
    and returns @p@, @u'@\;
    implies a read barrier
-}
{-# INLINE atomicExchangeWordAddr# #-}
atomicExchangeWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
atomicExchangeWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u i ->
        case GHC.atomicExchangeWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u0@, @u1@, @i@,
    atomically
    reads @u'@ off @p + repBytes Word8Rep * i bytes@,
    writes @u1@ to @p@ iff @u0@ agrees with @u@
    (doing nothing otherwise),
    and returns @p@, @u@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord8Addr# #-}
atomicCasWord8Addr# ::
    forall t. Addr# t %One-> Word8# %One-> Word8# %One-> Int# %One-> (# Addr# t, Word8# #)
atomicCasWord8Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u0 u1 i ->
        case GHC.atomicCasWord8Addr# (GHC.plusAddr# a (repBytes Word8Rep *# i)) u0 u1 s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u0@, @u1@, @i@,
    atomically
    reads @u'@ off @p + repBytes Word8Rep * i bytes@,
    writes @u1@ to @p@ iff @u0@ agrees with @u@
    (doing nothing otherwise),
    and returns @p@, @u@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord16Addr# #-}
atomicCasWord16Addr# ::
    forall t. Addr# t %One-> Word16# %One-> Word16# %One-> Int# %One-> (# Addr# t, Word16# #)
atomicCasWord16Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u0 u1 i ->
        case GHC.atomicCasWord16Addr# (GHC.plusAddr# a (repBytes Word16Rep *# i)) u0 u1 s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u0@, @u1@, @i@,
    atomically
    reads @u'@ off @p + repBytes Word8Rep * i bytes@,
    writes @u1@ to @p@ iff @u0@ agrees with @u@
    (doing nothing otherwise),
    and returns @p@, @u@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord32Addr# #-}
atomicCasWord32Addr# ::
    forall t. Addr# t %One-> Word32# %One-> Word32# %One-> Int# %One-> (# Addr# t, Word32# #)
atomicCasWord32Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u0 u1 i ->
        case GHC.atomicCasWord32Addr# (GHC.plusAddr# a (repBytes Word32Rep *# i)) u0 u1 s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u0@, @u1@, @i@,
    atomically
    reads @u'@ off @p + repBytes Word8Rep * i bytes@,
    writes @u1@ to @p@ iff @u0@ agrees with @u@
    (doing nothing otherwise),
    and returns @p@, @u@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWord64Addr# #-}
atomicCasWord64Addr# ::
    forall t. Addr# t %One-> Word64# %One-> Word64# %One-> Int# %One-> (# Addr# t, Word64# #)
atomicCasWord64Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u0 u1 i ->
        case GHC.atomicCasWord64Addr# (GHC.plusAddr# a (repBytes Word64Rep *# i)) u0 u1 s of
            (# s', u' #) -> (# (# s', a #), u' #)

{- | Given arguments @p@, @u0@, @u1@, @i@,
    atomically
    reads @u'@ off @p + repBytes Word8Rep * i bytes@,
    writes @u1@ to @p@ iff @u0@ agrees with @u@
    (doing nothing otherwise),
    and returns @p@, @u@\;
    implies a full memory barrier
-}
{-# INLINE atomicCasWordAddr# #-}
atomicCasWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Word# %One-> Int# %One-> (# Addr# t, Word# #)
atomicCasWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce $ \ (# s, a #) u0 u1 i ->
        case GHC.atomicCasWordAddr# (GHC.plusAddr# a (repBytes WordRep *# i)) u0 u1 s of
            (# s', u' #) -> (# (# s', a #), u' #)