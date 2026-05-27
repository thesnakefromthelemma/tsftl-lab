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
  , ViewPatterns
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

  * Prove the soundness of not threading 'State#' and passing 'realWorld#' below

  * Expose 'GHC.NullAddr#', simplifying 'NullAddr#'

  * Resolve issue #18472, allowing the below FFI imports to be greatly simplified

  * Upgrade GHC's SIMD support (cf. issue #25030)

  * Case SIMD support on more host archs (cf. "GHC.Platform.ArchOS")
-}

{- | 'State#'-parametrized machine addresses -}
module Data.Addr.Linear
  ( -- * 'State#'-parametrized machine addresses
    Addr#
    -- * Linear (i.e., non-GC, foreign heap) bytearray (a/rea/dea)llocation via 'Addr#'s
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
  , realWorld#
  , pattern One
  , pattern Many
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
#if SIMD
  , repBytes
  , supportedSIMDBytes
#endif
  )

import Prelude.Linear
  ( Ur
  , ur
  )

import Data.State.Linear
  ( LAlloc# )

import Data.State.Linear.Unsafe
  ( pattern LAlloc# )

import Data.Addr.Linear.TH
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

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "allocAddrBytesPrimOp"
    allocAddrBytes_primOp# ::
        forall t. Int# %Many-> LAlloc# t %Many-> Addr# t
{- | Given argument @n@,
    allocates @n@ bytes on the foreign heap at address @p@,
    returning @p@\;
    wraps a @ccall@ to @malloc@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs an allocation without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'allocAddrBytes_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE allocAddrBytes# #-}
allocAddrBytes# ::
    forall t. Int# %One-> LAlloc# t %One-> Addr# t
allocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> allocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "allocAddrBytesAlignedPrimOp"
    allocAddrBytesAligned_primOp# ::
        forall t. Int# %Many-> Int# %Many-> LAlloc# t %Many-> Addr# t
{- | Given arguments @n@, @d@,
    allocates @n@ bytes of alignment @d@ on the foreign heap at address @p@,
    returning @p@\;
    wraps a @ccall@ to @alloc_aligned@\;
    assumes that @n@ is a multiple of @d@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs an allocation without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'allocAddrBytesAligned_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE allocAddrBytesAligned# #-}
allocAddrBytesAligned# ::
    forall t. Int# %One-> Int# %One-> LAlloc# t %One-> Addr# t
allocAddrBytesAligned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> allocAddrBytesAligned_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "callocAddrBytesPrimOp"
    callocAddrBytes_primOp# ::
        forall t. Int# %Many-> LAlloc# t %Many-> Addr# t
{- | Given argument @n@,
    allocates @n@ zeroed bytes on the foreign heap at address @p@,
    returning @p@\;
    wraps a @ccall@ to @calloc@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs an allocation without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'callocAddrBytes_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE callocAddrBytes# #-}
callocAddrBytes# ::
    forall t. Int# %One-> LAlloc# t %One-> Addr# t
callocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> callocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "callocAddrBytesAlignedPrimOp"
    callocAddrBytesAligned_primOp# ::
        forall t. Int# %Many-> Int# %Many-> LAlloc# t %Many-> Addr# t
{- | Given arguments @n@, @d@,
    allocates @n@ zeroed bytes of alignment @d@ on the foreign heap at address @p@,
    returning @p@\;
    wraps a @ccall@ to @calloc_aligned@ and a @prim@ call to @memset@\;
    assumes that @n@ is a multiple of @d@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs an allocation without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'callocAddrBytesAligned_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE callocAddrBytesAligned# #-}
callocAddrBytesAligned# ::
    forall t. Int# %One-> Int# %One-> LAlloc# t %One-> Addr# t
callocAddrBytesAligned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> callocAddrBytesAligned_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "reallocAddrBytesPrimOp"
    reallocAddrBytes_primOp# ::
        forall t. Addr# t %Many-> Int# %Many-> Addr# t
{- | Given arguments @p@, @n@,
    resizes @p@\'s allocation to @n@ bytes,
    returning @p@\;
    wraps a @ccall@ to @realloc@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a reallocation without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'reallocAddrBytes_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE reallocAddrBytes# #-}
reallocAddrBytes# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
reallocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> reallocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "freeAddrPrimOp"
    freeAddr_primOp# ::
        forall t. Addr# t %Many-> LAlloc# t
{- | Given argument @p@,
    frees @p@\'s allocation,
    returning a 'State#'-parametrized linear allocation token\;
    wraps a @ccall@ to @free@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping performs a free without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'freeAddr_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE freeAddr# #-}
freeAddr# ::
    forall t. Addr# t %One-> LAlloc# t
freeAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> freeAddr_primOp#


-- * Basic bulk byte manipulations

foreign import prim "setAddrBytesPrimOp"
    setAddrBytesSigned_primOp# ::
        forall t. Addr# t %Many-> Int# %Many-> Int8# %Many-> (# #)
{- | Given arguments @p@, @n@, @c@,
    sets the first @n@ bytes off @p@ to @c@,
    returning @p@\;
    wraps a @prim@ call to @memset@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a byteset without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'setAddrBytesSigned_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE setAddrBytesSigned# #-}
setAddrBytesSigned# ::
    forall t. Addr# t %One-> Int# %One-> Int8# %One-> Addr# t
setAddrBytesSigned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p n c ->
        case setAddrBytesSigned_primOp# p n c of
            _ -> p

foreign import prim "setAddrBytesPrimOp"
    setAddrBytesUnsigned_primOp# ::
        forall t. Addr# t %Many-> Int# %Many-> Word8# %Many-> (# #)
{- | Given arguments @p@, @n@, @c@,
    sets the first @n@ bytes off @p@ to @c@,
    returning @p@\;
    wraps a @prim@ call to @memset@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a byteset without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'setAddrBytesUnsigned_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE setAddrBytesUnsigned# #-}
setAddrBytesUnsigned# ::
    forall t. Addr# t %One-> Int# %One-> Word8# %One-> Addr# t
setAddrBytesUnsigned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p n c ->
        case setAddrBytesUnsigned_primOp# p n c of
            _ -> p

foreign import prim "copyAddrBytesPrimOp"
    copyAddrBytes_primOp# ::
        forall t. Addr# t %Many-> Addr# t %Many-> Int# %Many-> (# #)
{- | Given arguments @p_src@, @p_dst@, @n@
    (where the two ranges may overlap),
    copies the first @n@ bytes off @p_src@ to the first @n@ bytes off @p_dst@,
    returning @p_src@, @p_dst@\;
    wraps a @prim@ call to @memmove@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a bytecopy without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'copyAddrBytes_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE copyAddrBytes# #-}
copyAddrBytes# ::
    forall t. Addr# t %One-> Addr# t %One-> Int# %One-> (# Addr# t, Addr# t #)
copyAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p_src p_dst n ->
        case copyAddrBytes_primOp# p_src p_dst n of
            _ -> (# p_src, p_dst #)

foreign import prim "copyAddrNonOverlappingBytesPrimOp"
    copyAddrNonOverlappingBytes_primOp# ::
        forall t. Addr# t %Many-> Addr# t %Many-> Int# %Many-> (# #)
{- | Given arguments @p_src@, @p_dst@,@p_src@, @p_dst@ @n@,
    where they two ranges are assumed to not overlap,
    copies the first @n@ bytes off @p_src@ to the first @n@ bytes off @p_dst@,
    returning @p_src@, @p_dst@\;
    wraps a @prim@ call to @memcpy@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a bytecopy without threading any 'State#' tokens,
    the persistence and sequencing of this effect enforced only by
    that 'copyAddrBytesNonOverlapping_primOp#' is marked as @has_side_effects = True@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE copyAddrNonOverlappingBytes# #-}
copyAddrNonOverlappingBytes# ::
    forall t. Addr# t %One-> Addr# t %One-> Int# %One-> (# Addr# t, Addr# t #)
copyAddrNonOverlappingBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p_src p_dst n ->
        case copyAddrNonOverlappingBytes_primOp# p_src p_dst n of
            _ -> (# p_src, p_dst #)


-- * 'Addr#' arithmetic

-- _ Thanks to Jaror for this idea!
{- | The null address -}
pattern NullAddr# :: forall t. Addr# t
pattern NullAddr# <- ((\ (Addr# a) -> GHC.eqAddr# GHC.nullAddr# a) -> 1#) where
    NullAddr# = Addr# GHC.nullAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is equal to @p1@ and @0#@ otherwise
-}
{-# INLINE eqAddr# #-}
eqAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> Int#
eqAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.eqAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is not equal to @p1@ and @0#@ otherwise
-}
{-# INLINE neAddr# #-}
neAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> Int#
neAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.neAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE geAddr# #-}
geAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> Int#
geAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.geAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than @p1@ and @0#@ otherwise
-}
{-# INLINE gtAddr# #-}
gtAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> Int#
gtAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.gtAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE leAddr# #-}
leAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> Int#
leAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.leAddr#

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than @p1@ and @0#@ otherwise
-}
{-# INLINE ltAddr# #-}
ltAddr# ::
    forall t. Addr# t %One-> Addr# t %One-> Int#
ltAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.ltAddr#

{- | Given arguments @p@, @n@,
    returns the machine address an offset of @n@ bytes from @p@
-}
{-# INLINE plusAddrBytes# #-}
plusAddrBytes# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
plusAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.plusAddr#

{- | Given arguments @p0@, @p1@,
    returns the offset of @p0@ from @p1@ in bytes
-}
{-# INLINE minusAddrBytes# #-}
minusAddrBytes# ::
    forall t. Addr# t %One-> Addr# t %One-> Int#
minusAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.minusAddr#

{- | Given arguments @p@, @m@,
    returns the remainder in bytes when @p@ is divided by @m@
-}
{-# INLINE remAddrBytes# #-}
remAddrBytes# ::
    forall t. Addr# t %One-> Int# %One-> Int#
remAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> coerce GHC.remAddr#


-- * Prefetching via 'Addr#'s

{- | Given argument @p@,
    prefetches @p@ to a register,
    returning @p@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a prefetch by consuming 'realWorld#',
    the persistence and sequencing of this effect enforced only by
    that 'GHC.prefetchAddr0#' is marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that the case expression scrutinizing their result
    must be forced when consuming its result\;
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE prefetchAddr0# #-}
prefetchAddr0# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr0# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.prefetchAddr0# a n realWorld# of
            _ -> p

{- | Given argument @p@,
    prefetches @p@ to the L1 cache,
    returning @p@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a prefetch by consuming 'realWorld#',
    the persistence and sequencing of this effect enforced only by
    that 'GHC.prefetchAddr1#' is marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE prefetchAddr1# #-}
prefetchAddr1# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr1# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.prefetchAddr1# a n realWorld# of
            _ -> p

{- | Given argument @p@,
    prefetches @p@ to the L2 cache,
    returning @p@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a prefetch by consuming 'realWorld#',
    the persistence and sequencing of this effect enforced only by
    that 'GHC.prefetchAddr2#' is marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE prefetchAddr2# #-}
prefetchAddr2# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr2# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.prefetchAddr2# a n realWorld# of
            _ -> p

{- | Given argument @p@,
    prefetches @p@ to the L3 cache,
    returning @p@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the wrapping code performs a prefetch by consuming 'realWorld#',
    the persistence and sequencing of this effect enforced only by
    that 'GHC.prefetchAddr3#' is marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE prefetchAddr3# #-}
prefetchAddr3# ::
    forall t. Addr# t %One-> Int# %One-> Addr# t
prefetchAddr3# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.prefetchAddr3# a n realWorld# of
            _ -> p


-- * Writing/reading off 'Addr#'s

{- | Instantiates 'Addrable' for various 'RuntimeRep's -}
$(sequence $ do
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
            let r = VecRep c e
            case Prelude.elem (I# (repBytes r)) supportedSIMDBytes of
                True  -> [ deriveAddrable r ]
                False -> [ ]
#else
        Vec  -> [ ]
#endif
        Box  -> [ ]
  )

{- | Given arguments @p@, @n@, @c@,
    where @c@ is assumed to be @1@ byte,
    writes @c@ to @p@ at an offset of @n@ bytes,
    returning @p@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a write by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE writeCharOffAddr# #-}
writeCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> Char# %One-> Addr# t
writeCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n c ->
        case GHC.writeCharOffAddr# a n c realWorld# of
            _ -> p

{- | Given arguments @p@, @n@, @c@,
    where @c@ is assumed to be @4@ bytes,
    returns the 'State#' action
    writing @c@ to @p@ at an offset of @4 * n@ bytes,
    returning @p@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a write by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE writeWideCharOffAddr# #-}
writeWideCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> Char# %One-> Addr# t
writeWideCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n c ->
        case GHC.writeWideCharOffAddr# a n c realWorld# of
            _ -> p

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading @c@ from @p@ at an offset of @n@ bytes,
    where @c@ is assumed to be @1@ byte,
    returning @p@, @c@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a read by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE readCharOffAddr# #-}
readCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> (# Addr# t, Ur Char# #)
readCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.readCharOffAddr# a n realWorld# of
            (# _, c #) -> (# p, ur c #)
{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading @c@ from @p@ at an offset of @4 * n@ bytes,
    returning @p@, @c@

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a read by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE readWideCharOffAddr# #-}
readWideCharOffAddr# ::
    forall t. Addr# t %One-> Int# %One-> (# Addr# t, Ur Char# #)
readWideCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.readWideCharOffAddr# a n realWorld# of
            (# _, c #) -> (# p, ur c #)


-- * Interoperation with GHC's 'ByteArray#'/'MutableByteArray#'

{- | Given arguments @w@, @i@, @p@, @n@,
    returns the linear 'State#' action
    copying the first @n@ bytes from offset @i@ bytes of @w@ to @p@,
    returning @p@
-}
{-# INLINE copyMutableByteArrayToAddr# #-}
copyMutableByteArrayToAddr# ::
    forall t s.
    MutableByteArray# s %One-> Int# %One-> Addr# t %One-> Int# %One->
    State# s %One-> (# State# s, Addr# t #)
copyMutableByteArrayToAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ w i p@(Addr# a) n s0 ->
        case GHC.copyMutableByteArrayToAddr# w i a n s0 of
            s1 -> (# s1, p #)

{- | Given arguments @v@, @i@, @p@, @n@,
    returns the linear 'State#' action
    copying the first @n@ bytes from offset @i@ bytes of @w@ to @p@
-}
{-# INLINE copyByteArrayToAddr# #-}
copyByteArrayToAddr# ::
    forall t s.
    ByteArray# %One-> Int# %One-> Addr# t %One-> Int# %One->
    State# s %One-> (# State# s, Addr# t #)
copyByteArrayToAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ v i p@(Addr# a) n s0 ->
        case GHC.copyByteArrayToAddr# v i a n s0 of
            s1 -> (# s1, p #)

{- | Given arguments @p@, @w@, @i@, @n@,
    returns the linear 'State#' action
    copying the first @n@ bytes from @p@ to an offset of @i@ bytes of @w@
-}
{-# INLINE copyAddrToMutableByteArray# #-}
copyAddrToMutableByteArray# ::
    forall t s.
    Addr# t %One-> MutableByteArray# s %One-> Int# %One-> Int# %One->
    State# s %One-> (# State# s, Addr# t #)
copyAddrToMutableByteArray# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) w i n s0 ->
        case GHC.copyAddrToByteArray# a w i n s0 of
            s1 -> (# s1, p #)


-- * Concurrency primitives

{- | Given arguments @p@, @n@,
    atomically
    writes @n@ to @p@,
    returning @p@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs an atomic write by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicWriteWordAddr# #-}
atomicWriteWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Addr# t
atomicWriteWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.atomicWriteWordAddr# a n realWorld# of
            _ -> p


{- | Given argument @p@, atomically
    atomically
    reads @n@ off @p@,
    returning @p@, @n@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs an atomic read by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicReadWordAddr# #-}
atomicReadWordAddr# ::
    forall t. Addr# t %One-> (# Addr# t, Ur Word# #)
atomicReadWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) ->
        case GHC.atomicReadWordAddr# a realWorld# of
            (# _, n #) -> (# p, ur n #)

{- | Given arguments @p@, @n@,
    atomically
    reads @n'@ off @p@,
    computes @n'' := n XOR n'@,
    and writes @n''@ to @p@,
    returning @p@, @n'@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a fetch-and-XOR by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE fetchXorWordAddr# #-}
fetchXorWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> (# Addr# t, Ur Word# #)
fetchXorWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.fetchXorWordAddr# a n realWorld# of
            (# _, n' #) -> (# p, ur n' #)

{- | Given arguments @p@, @n@,
    atomically
    reads @n'@ off @p@,
    computes @n'' := n AND n'@,
    and writes @n''@ to @p@,
    returning @p@, @n'@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a fetch-and-AND by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE fetchAndWordAddr# #-}
fetchAndWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> (# Addr# t, Ur Word# #)
fetchAndWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.fetchAndWordAddr# a n realWorld# of
            (# _, n' #) -> (# p, ur n' #)

{- | Given arguments @p@, @n@,
    atomically
    reads @n'@ off @p@,
    computes @n'' := n NAND n'@,
    and writes @n''@ to @p@,
    returning @p@, @n'@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a fetch-and-NAND by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE fetchNandWordAddr# #-}
fetchNandWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> (# Addr# t, Ur Word# #)
fetchNandWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.fetchNandWordAddr# a n realWorld# of
            (# _, n' #) -> (# p, ur n' #)

{- | Given arguments @p@, @n@,
    atomically
    reads @n'@ off @p@,
    computes @n'' := n OR n'@,
    and writes @n''@ to @p@,
    returning @p@, @n'@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a fetch-and-OR by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE fetchOrWordAddr# #-}
fetchOrWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> (# Addr# t, Ur Word# #)
fetchOrWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.fetchOrWordAddr# a n realWorld# of
            (# _, n' #) -> (# p, ur n' #)

{- | Given arguments @p@, @n@,
    atomically
    reads @n'@ off @p@,
    computes @n'' := n + n'@,
    and writes @n''@ to @p@,
    returning @p@, @n'@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a fetch-and-add by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE fetchAddWordAddr# #-}
fetchAddWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> (# Addr# t, Ur Word# #)
fetchAddWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.fetchAddWordAddr# a n realWorld# of
            (# _, n' #) -> (# p, ur n' #)

{- | Given arguments @p@, @n@,
    atomically
    reads @n'@ off @p@,
    computes @n'' := n' - n@,
    and writes @n''@ to @p@,
    returning @p@, @n'@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a fetch-and-sub by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE fetchSubWordAddr# #-}
fetchSubWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> (# Addr# t, Ur Word# #)
fetchSubWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.fetchSubWordAddr# a n realWorld# of
            (# _, n' #) -> (# p, ur n' #)

{- | Given arguments @p@, @n@,
    atomically
    reads @n'@ off @p@,
    and writes @n@ to @p@,
    returning @p@, @n'@\;
    implies a read barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs an atomic exchange by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicExchangeWordAddr# #-}
atomicExchangeWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> (# Addr# t, Ur Word# #)
atomicExchangeWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.atomicExchangeWordAddr# a n realWorld# of
            (# _, n' #) -> (# p, ur n' #)

{- | Given arguments @p@, @n0@, @n1@,
    atomically
    reads @n@ off @p@,
    and writes @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    returning @p@, @n@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a cas by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicCasWord8Addr# #-}
atomicCasWord8Addr# ::
    forall t. Addr# t %One-> Word8# %One-> Word8# %One-> (# Addr# t, Ur Word8# #)
atomicCasWord8Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n0 n1 ->
        case GHC.atomicCasWord8Addr# a n0 n1 realWorld# of
            (# _, n #) -> (# p, ur n #)

{- | Given arguments @p@, @n0@, @n1@,
    atomically
    reads @n@ off @p@,
    and writes @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    returning @p@, @n@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a cas by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicCasWord16Addr# #-}
atomicCasWord16Addr# ::
    forall t. Addr# t %One-> Word16# %One-> Word16# %One-> (# Addr# t, Ur Word16# #)
atomicCasWord16Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n0 n1 ->
        case GHC.atomicCasWord16Addr# a n0 n1 realWorld# of
            (# _, n #) -> (# p, ur n #)

{- | Given arguments @p@, @n0@, @n1@,
    atomically
    reads @n@ off @p@,
    and writes @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    returning @p@, @n@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a cas by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicCasWord32Addr# #-}
atomicCasWord32Addr# ::
    forall t. Addr# t %One-> Word32# %One-> Word32# %One-> (# Addr# t, Ur Word32# #)
atomicCasWord32Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n0 n1 ->
        case GHC.atomicCasWord32Addr# a n0 n1 realWorld# of
            (# _, n #) -> (# p, ur n #)

{- | Given arguments @p@, @n0@, @n1@,
    atomically
    reads @n@ off @p@,
    and writes @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    returning @p@, @n@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a cas by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicCasWord64Addr# #-}
atomicCasWord64Addr# ::
    forall t. Addr# t %One-> Word64# %One-> Word64# %One-> (# Addr# t, Ur Word64# #)
atomicCasWord64Addr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n0 n1 ->
        case GHC.atomicCasWord64Addr# a n0 n1 realWorld# of
            (# _, n #) -> (# p, ur n #)

{- | Given arguments @p@, @n0@, @n1@,
    atomically
    reads @n@ off @p@,
    and writes @n1@ to @p@ iff @n0@ agrees with @n@
    (doing nothing otherwise),
    returning @p@, @n@\;
    implies a full memory barrier

    WARNING: In the interest of simplicity (especially re the kind of 'Addr#')
    and encouraging desirable optimizations,
    the generated code performs a cas by consuming 'realWorld#',
    the persistence and sequencing of those effects enforced only by
    that the underlying primops are marked as @has_side_effects = True@
    in @ghc/compiler/GHC/Builtin/primops.txt.pp@
    and that it has unlifted return type,
    hence that any expression scrutinizing its result must first force it\;
    note that the consumed and returned 'Addr#' values are otherwise equal.
    I cannot pretend to (yet) be 100% convinced that the above is semantically sound!
    Should something go wrong, look here first...
-}
{-# INLINE atomicCasWordAddr# #-}
atomicCasWordAddr# ::
    forall t. Addr# t %One-> Word# %One-> Word# %One-> (# Addr# t, Ur Word# #)
atomicCasWordAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n0 n1 ->
        case GHC.atomicCasWordAddr# a n0 n1 realWorld# of
            (# _, n #) -> (# p, ur n #)