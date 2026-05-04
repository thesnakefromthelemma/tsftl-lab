{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTs
  , KindSignatures
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wno-inaccessible-code #-}

{- | 'State#'-parametrized machine addresses -}
module Data.Addr.Linear
  ( -- * 'State#'-parametrized machine addresses
    Addr#
      ( Addr# )
    -- * Writing off of 'Addr#'
  , writeAddrOffAddr# {-
  , writeCharOffAddr#
  , writeWideCharOffAddr#
  , writeIntOffAddr#
  , writeWordOffAddr#
  , writeFloatOffAddr#
  , writeDoubleOffAddr#
  , writeInt8OffAddr#
  , writeWord8OffAddr#
  , writeInt16OffAddr#
  , writeWord16OffAddr#
  , writeInt32OffAddr#
  , writeWord32OffAddr#
  , writeInt64OffAddr#
  , writeWord64OffAddr#
  , writeInt8X16OffAddr#
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
  , writeDoubleX8OffAddr# -}
    -- * Reading off of 'Addr#'
  , readAddrOffAddr# {-
  , readCharOffAddr#
  , readWideCharOffAddr#
  , readIntOffAddr#
  , readWordOffAddr#
  , readFloatOffAddr#
  , readDoubleOffAddr#
  , readInt8OffAddr#
  , readWord8OffAddr#
  , readInt16OffAddr#
  , readWord16OffAddr#
  , readInt32OffAddr#
  , readWord32OffAddr#
  , readInt64OffAddr#
  , readWord64OffAddr#
  , readInt8X16OffAddr#
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
  , readDoubleX8OffAddr# -}
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , pattern Many
  , pattern One
  , State#
  , realWorld#
  , Char#
  , Int#
  , Word#
  , Float#
  , Double#
  , StablePtr#
  , Int8#
  , Word8#
  , Int16#
  , Word16#
  , Int32#
  , Word32#
  , Int64#
  , Word64#
  , Int8X16#
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
  , DoubleX8#
  )

import qualified GHC.Exts as RealWorld
  ( Addr#
  , writeCharOffAddr#
  , writeWideCharOffAddr#
  , writeIntOffAddr#
  , writeWordOffAddr#
  , writeAddrOffAddr#
  , writeFloatOffAddr#
  , writeDoubleOffAddr#
  , writeStablePtrOffAddr#
  , writeInt8OffAddr#
  , writeWord8OffAddr#
  , writeInt16OffAddr#
  , writeWord16OffAddr#
  , writeInt32OffAddr#
  , writeWord32OffAddr#
  , writeInt64OffAddr#
  , writeWord64OffAddr#
  , writeInt8X16OffAddr#
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
  , writeDoubleX8OffAddr#
  , readCharOffAddr#
  , readWideCharOffAddr#
  , readIntOffAddr#
  , readWordOffAddr#
  , readAddrOffAddr#
  , readFloatOffAddr#
  , readDoubleOffAddr#
  , readStablePtrOffAddr#
  , readInt8OffAddr#
  , readWord8OffAddr#
  , readInt16OffAddr#
  , readWord16OffAddr#
  , readInt32OffAddr#
  , readWord32OffAddr#
  , readInt64OffAddr#
  , readWord64OffAddr#
  , readInt8X16OffAddr#
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
  , readDoubleX8OffAddr#
  )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ (internal)

import Prelude.Linear
  ( Ur
  , ur
  )

import Data.Addr.Linear.Internal
  ( Addr#
      ( Addr# )
  , deriveAddrOps
  )


-- * TH-driven derivation of 'Addr#' w/r ops

{- | Type synonym for derivation -}
type WideChar# = Char#

{- | Given arguments @p@, @n@, @x@,
    linearly consumes @p@, writing @x@ thereto at an offset of @n@ bytes,
    the result a fresh instance of @p@
-}
writeAddrOffAddr# :: forall s. Addr# s %1 -> Int# %1 -> Addr# s %1 -> Addr# s
writeAddrOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# q) n (Addr# x) ->
        case RealWorld.writeAddrOffAddr# q n x realWorld# of
            _ -> p

{- | Given arguments @p@, @n@, @x@,
    linearly consumes @p@, reading therefrom at an offset of @n@ bytes,
    the results a fresh instance of @p@ and the read value in that order
-}
readAddrOffAddr# :: forall s. Addr# s %1 -> Int# %1 -> (# Addr# s, Ur (Addr# s) #)
readAddrOffAddr# = case unsafeEqualityProof @Many @One of
    UnsafeRefl -> \ p@(Addr# q) n ->
        case RealWorld.readAddrOffAddr# q n realWorld# of
            (# _, x #) -> (# p, ur (Addr# x) #)