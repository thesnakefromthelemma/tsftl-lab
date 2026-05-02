{-# LANGUAGE Haskell2010
  , DataKinds
  , GADTs
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | 'State#'-parametrized machine addresses -}
module Data.Addr
  ( -- * 'State#'-parametrized machine addresses
    Addr#
      ( Addr# )
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , pattern Lifted
  , pattern BoxedRep
  , pattern AddrRep
  )

import GHC.Exts
  ( TYPE
  , pattern Lifted
  , pattern BoxedRep
  , pattern AddrRep
  , State#
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


-- _ Type synonym make CPP derivation work

type WideChar# = Char#


-- * 'State#'-parametrized machine addresses

newtype Addr# :: TYPE (BoxedRep Lifted) -> TYPE AddrRep where
    Addr# :: forall s. RealWorld.Addr# -> Addr# s