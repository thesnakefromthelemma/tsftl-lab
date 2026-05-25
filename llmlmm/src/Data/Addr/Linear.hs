{-# LANGUAGE Haskell2010
  , DataKinds
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

{- | 'State#'-parametrized machine addresses -}
module Data.Addr.Linear
  ( -- * 'State#'-parametrized machine addresses
    Addr#
    -- * Linear (i.e., foreign heap, non-GC) bytearray (a/rea/dea)llocation via 'Addr#'s
  , allocAddrBytes#
  , callocAddrBytes#
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
  , realWorld#
  , pattern One
  , pattern Many
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
{-, repBytes
  , supportedSIMDBytes-}
  )

import Prelude.Linear
  ( Ur
  , ur
  )

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


-- * Manual (i.e., foreign heap, non-GC) bytearray (a/rea/dea)llocation via 'Addr#'s

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "allocAddrBytesPrimOp"
    allocAddrBytes_primOp# ::
        forall s. Int# %Many-> State# s %Many-> Addr# s
{- | Given argument @n@,
    returns the 'State#' action
    allocating @n@ bytes on the foreign heap,
    its result the machine address of the allocation\;
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
    forall s. Int# %One-> State# s %One-> Addr# s
allocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> allocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "callocAddrBytesPrimOp"
    callocAddrBytes_primOp# ::
        forall s. Int# %Many-> State# s %Many-> Addr# s
{- | Given argument @n@,
    returns the 'State#' action
    allocating @n@ zeroed bytes on the foreign heap,
    its result the machine address of the allocation\;
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
    forall s. Int# %One-> State# s %One-> Addr# s
callocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> callocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "reallocAddrBytesPrimOp"
    reallocAddrBytes_primOp# ::
        forall s. Addr# s %Many-> Int# %Many-> Addr# s
{- | Given arguments @p@, @n@,
    returns the 'State#' action
    resizing @p@\'s allocation to @n@ bytes,
    its result the machine address of the resized allocation\;
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
    forall s. Addr# s %One-> Int# %One-> Addr# s
reallocAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> reallocAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "freeAddrPrimOp"
    freeAddr_primOp# ::
        forall s. Addr# s %Many-> (# #)
{- | Given argument @p@,
    returns the 'State#' action
    freeing @p@\'s allocation\;
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
    forall s. Addr# s %One-> (# #)
freeAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> freeAddr_primOp#


-- * Basic bulk byte manipulations

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesSigned_primOp# ::
        forall s. Addr# s %Many-> Int# %Many-> Int8# %Many-> Addr# s
{- | Given arguments @p@, @n@, @c@
    returns the 'State#' action
    setting the first @n@ bytes off @p@ to @c@\;
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
    forall s. Addr# s %One-> Int# %One-> Int8# %One-> Addr# s
setAddrBytesSigned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> setAddrBytesSigned_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "setAddrBytesPrimOp"
    setAddrBytesUnsigned_primOp# ::
        forall s. Addr# s %Many-> Int# %Many-> Word8# %Many-> Addr# s
{- | Given arguments @p@, @n@, @c@
    returns the 'State#' action
    setting the first @n@ bytes off @p@ to @c@\;
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
    forall s. Addr# s %One-> Int# %One-> Word8# %One-> Addr# s
setAddrBytesUnsigned# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> setAddrBytesUnsigned_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "copyAddrBytesPrimOp"
    copyAddrBytes_primOp# ::
        forall s. Addr# s %Many-> Addr# s %Many-> Int# %Many-> Addr# s
{- | Given arguments @p_src@, @p_dst@ @n@,
    returns the 'State#' action
    copying the first @n@ bytes off @p_src@ to the first @n@ bytes off @p_dst@,
    where the two ranges may overlap\;
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
    forall s. Addr# s %One-> Addr# s %One-> Int# %One-> Addr# s
copyAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> copyAddrBytes_primOp#

{- _ Cf. @GHC-57396@ as to why this song and dance is necessary -}
foreign import prim "copyAddrNonOverlappingBytesPrimOp"
    copyAddrNonOverlappingBytes_primOp# ::
        forall s. Addr# s %Many-> Addr# s %Many-> Int# %Many-> Addr# s
{- | Given arguments @p_src@, @p_dst@ @n@,
    returns the 'State#' action
    copying the first @n@ bytes off @p_src@ to the first @n@ bytes off @p_dst@
    where the two ranges may not overlap\;
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
    forall s. Addr# s %One-> Addr# s %One-> Int# %One-> Addr# s
copyAddrNonOverlappingBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> copyAddrNonOverlappingBytes_primOp#


-- * 'Addr#' arithmetic

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is equal to @p1@ and @0#@ otherwise
-}
{-# INLINE eqAddr# #-}
eqAddr# ::
    forall s. Addr# s %One-> Addr# s %One-> Int#
eqAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a0) (Addr# a1) -> GHC.eqAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is not equal to @p1@ and @0#@ otherwise
-}
{-# INLINE neAddr# #-}
neAddr# ::
    forall s. Addr# s %One-> Addr# s %One-> Int#
neAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a0) (Addr# a1) -> GHC.neAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE geAddr# #-}
geAddr# ::
    forall s. Addr# s %One-> Addr# s %One-> Int#
geAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a0) (Addr# a1) -> GHC.geAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is greater than @p1@ and @0#@ otherwise
-}
{-# INLINE gtAddr# #-}
gtAddr# ::
    forall s. Addr# s %One-> Addr# s %One-> Int#
gtAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a0) (Addr# a1) -> GHC.gtAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than or equal to @p1@ and @0#@ otherwise
-}
{-# INLINE leAddr# #-}
leAddr# ::
    forall s. Addr# s %One-> Addr# s %One-> Int#
leAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a0) (Addr# a1) -> GHC.leAddr# a0 a1

{- | Given arguments @p0@, @p1@,
    returns @1#@ if @p0@ is less than @p1@ and @0#@ otherwise
-}
{-# INLINE ltAddr# #-}
ltAddr# ::
    forall s. Addr# s %One-> Addr# s %One-> Int#
ltAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a0) (Addr# a1) -> GHC.ltAddr# a0 a1

{- | Given arguments @p@, @n@,
    returns the machine address an offset of @n@ bytes from @p@
-}
{-# INLINE plusAddrBytes# #-}
plusAddrBytes# ::
    forall s. Addr# s %One-> Int# %One-> Addr# s
plusAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a) n -> Addr# (GHC.plusAddr# a n)

{- | Given arguments @p0@, @p1@,
    returns the offset of @p0@ from @p1@ in bytes
-}
{-# INLINE minusAddrBytes# #-}
minusAddrBytes# ::
    forall s. Addr# s %One-> Addr# s %One-> Int#
minusAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a0) (Addr# a1) -> GHC.minusAddr# a0 a1

{- | Given arguments @p@, @m@,
    returns the remainder in bytes when @p@ is divided by @m@
-}
{-# INLINE remAddrBytes# #-}
remAddrBytes# ::
    forall s. Addr# s %One-> Int# %One-> Int#
remAddrBytes# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a) m -> GHC.remAddr# a m


-- * Prefetching via 'Addr#'s

{- | Given argument @p@,
    prefetches @p@ to a register

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
    forall s. Addr# s %One-> Int# %One-> (# #)
prefetchAddr0# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a) n ->
        case GHC.prefetchAddr0# a n realWorld# of
            _ -> (# #)

{- | Given argument @p@,
    prefetches @p@ to the L1 cache

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
    forall s. Addr# s %One-> Int# %One-> (# #)
prefetchAddr1# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a) n ->
        case GHC.prefetchAddr1# a n realWorld# of
            _ -> (# #)

{- | Given argument @p@,
    prefetches @p@ to the L2 cache

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
    forall s. Addr# s %One-> Int# %One-> (# #)
prefetchAddr2# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a) n ->
        case GHC.prefetchAddr2# a n realWorld# of
            _ -> (# #)

{- | Given argument @p@,
    prefetches @p@ to the L3 cache

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
    forall s. Addr# s %One-> Int# %One-> (# #)
prefetchAddr3# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ (Addr# a) n ->
        case GHC.prefetchAddr3# a n realWorld# of
            _ -> (# #)


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
    forall s. Addr# s %One-> Int# %One-> Char# %One-> Addr# s
writeCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n c ->
        case GHC.writeCharOffAddr# a n c realWorld# of
            _ -> p

{- | Given arguments @p@, @n@, @c@,
    returns the 'State#' action
    writing @c@ to @p@ at an offset of @4 * n@ bytes,
    where @c@ is assumed to be @4@ bytes

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
    forall s. Addr# s %One-> Int# %One-> Char# %One-> Addr# s
writeWideCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n c ->
        case GHC.writeWideCharOffAddr# a n c realWorld# of
            _ -> p

{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading from @p@ at an offset of @n@ bytes,
    where @c@ is assumed to be @1@ byte

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
    forall s. Addr# s %One-> Int# %One-> (# Addr# s, Ur Char# #)
readCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.readCharOffAddr# a n realWorld# of
            (# _, c #) -> (# p, ur c #)
{- | Given arguments @p@, @n@,
    returns the 'State#' action
    reading from @p@ at an offset of @4 * n@ bytes,
    where @c@ is assumed to be @4@ bytes

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
    forall s. Addr# s %One-> Int# %One-> (# Addr# s, Ur Char# #)
readWideCharOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# a) n ->
        case GHC.readWideCharOffAddr# a n realWorld# of
            (# _, c #) -> (# p, ur c #)