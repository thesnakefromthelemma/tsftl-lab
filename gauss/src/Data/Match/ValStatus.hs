{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , InstanceSigs
  , LambdaCase
  , MagicHash
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

module Data.Match.ValStatus
  ( -- * ValStatus
    ValStatus
      ( Dead
      , Alive
      )
  ) where


-- + Imports

-- ++ base

import GHC.Exts
  ( State#
  , Addr#
  , Int#
  , ByteArray#
  , MutableByteArray#
  )

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


-- * ValStatus

data ValStatus where
    Dead, Alive :: ValStatus

deriving stock instance Eq ValStatus
deriving stock instance Show ValStatus
deriving stock instance Ord ValStatus

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