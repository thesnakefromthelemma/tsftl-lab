{-# LANGUAGE Haskell2010
  , BangPatterns
  , CPP
  , DerivingStrategies
  , GADTSyntax
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , LambdaCase
  , MagicHash
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | 'ValStatus' represents the status of a \"value\"
    in the algorithm 'Match.match' in "Match"\;
    this module exports it, its constructors ('Dead' or 'Alive'),
    its 'Eq', 'Show', 'Ord', and the type 'ValStatusChunk'
    which allows 'ValStatus' values to be marshalled as
    the bits of a machine word for efficient packing/
    sequential access from/to (mutable) bytearrays
-}
module Data.Match.ValStatus
  ( -- * 'ValStatus'
    ValStatus
      ( Dead
      , Alive
      )
    -- * 'ValStatusChunk'
  , ValStatusChunk
      ( ValStatusChunk
      , unValStatusChunk )
  , valStatusChunkSize
  , valStatusChunkToList
  ) where


-- + Imports

-- ++ base

#include "MachDeps.h"

import GHC.Exts
  ( State#
  , Addr#
  , Int#
  , ByteArray#
  , MutableByteArray#
  )

import Data.Bits
  ( unsafeShiftR
  , (.&.)
  )

import qualified GHC.Exts as List
  ( build )

import Data.Word
  ( Word8 )

import Data.Proxy
  ( Proxy
      ( Proxy )
  )

-- ++ primitive

import Data.Primitive.Types
  ( Prim
      ( sizeOfType#
      , alignmentOfType#
      , indexByteArray#
      , readByteArray#
      , writeByteArray#
      , indexOffAddr#
      , readOffAddr#
      , writeOffAddr#
      )
  )


-- * 'ValStatus'

{- | Type representing the status of a \"val\" in the algorithm 'Match.match' in "Match" -}
data ValStatus where
    Dead, Alive :: ValStatus

deriving stock instance Eq ValStatus
deriving stock instance Show ValStatus
deriving stock instance Ord ValStatus


-- * 'ValStatusChunk'

{- | Because in our use case we marshall our 'ValStatus' values
    into a 'Data.Primitive.PrimArray' from which asymptotically
    100% of our reads are sequential (i.e., streaming), it is
    efficient to represent said 'ValStatus' values as bits of
    'Word'-sized chunks
-}
newtype ValStatusChunk where
    ValStatusChunk :: {
        unValStatusChunk :: Word } ->
        ValStatusChunk

deriving newtype instance Prim ValStatusChunk

{- | Word size in bits (platform-dependent)\;
    Presumably not in "GHC.Exts" only because of
    the lack of unlifted top-level bindings
-}
{-# INLINE valStatusChunkSize #-}
valStatusChunkSize :: Int
valStatusChunkSize = WORD_SIZE_IN_BITS

{- | Unfolds 'ValStatusChunk' to fold/build fusible
    'Data.List' of statically known length\;
    naively we should be able to speed this up
    by a factor of about \(n \leq 64\)
    at the expense of bloating code by a factor
    of about \(2^{n}\)
-}
{-# INLINE valStatusChunkToList #-}
valStatusChunkToList :: ValStatusChunk -> [ValStatus]
valStatusChunkToList = \ (ValStatusChunk a) -> List.build $
    \ g b ->
        let buildR = \ !a' n -> case compare valStatusChunkSize n of
                GT -> g (case 1 .&. a' of 0 -> Dead; _ -> Alive) $ buildR (unsafeShiftR a' 1) (n + 1)
                _  -> b
        in  buildR a 0


-- * TO DEPRECATE

{-# INLINE word8ToValStatus #-}
word8ToValStatus :: Word8 -> ValStatus
word8ToValStatus = \case
    0 -> Dead
    _ -> Alive

{-# INLINE valStatusToWord8 #-}
valStatusToWord8 :: ValStatus -> Word8
valStatusToWord8 = \case
    Dead  -> 0
    Alive -> 1

{- _ 'Data.Primitive.Types.Prim' instance of 'KeyStatus' -}
instance Prim ValStatus where
    {-# INLINE sizeOfType# #-}
    sizeOfType# ::
        Proxy ValStatus -> Int#
    sizeOfType# = \ Proxy ->
        sizeOfType# @Word8 Proxy

    {-# INLINE alignmentOfType# #-}
    alignmentOfType# ::
        Proxy ValStatus -> Int#
    alignmentOfType# = \ Proxy ->
        alignmentOfType# @Word8 Proxy

    {-# INLINE indexByteArray# #-}
    indexByteArray# ::
        ByteArray# -> Int# -> ValStatus
    indexByteArray# = \ ab i ->
        word8ToValStatus $ indexByteArray# ab i

    {-# INLINE readByteArray# #-}
    readByteArray# :: forall s.
        MutableByteArray# s -> Int# -> State# s -> (# State# s, ValStatus #)
    readByteArray# = \ mb i s ->
        case readByteArray# mb i s of
            (# s', b #) -> (# s', word8ToValStatus b #)

    {-# INLINE writeByteArray# #-}
    writeByteArray# :: forall s.
        MutableByteArray# s -> Int# -> ValStatus -> State# s -> State# s
    writeByteArray# = \ mb i v s ->
        writeByteArray# mb i (valStatusToWord8 v) s

    {-# INLINE indexOffAddr# #-}
    indexOffAddr# ::
        Addr# -> Int# -> ValStatus
    indexOffAddr# = \ x i ->
        word8ToValStatus $ indexOffAddr# x i

    {-# INLINE readOffAddr# #-}
    readOffAddr# :: forall s.
        Addr# -> Int# -> State# s -> (# State# s, ValStatus #)
    readOffAddr# = \ x i s ->
        case readOffAddr# x i s of
            (# s', b #) -> (# s', word8ToValStatus b #)

    {-# INLINE writeOffAddr# #-}
    writeOffAddr# :: forall s.
        Addr# -> Int# -> ValStatus -> State# s -> State# s
    writeOffAddr# = \ x i v s ->
        writeOffAddr# x i (valStatusToWord8 v) s