{-# LANGUAGE Haskell2010
{-, DerivingStrategies (redundant) -}
  , DerivingVia
  , GADTSyntax
  , InstanceSigs
  , LambdaCase
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
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


-- * Imports

-- ** base

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


-- ** primitive

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


-- ** vector

import Data.Vector.Generic.Mutable as W
  ( MVector )

import Data.Vector.Generic as V
  ( Vector )

import qualified Data.Vector.Primitive.Mutable as WP
  ( MVector )

import qualified Data.Vector.Primitive as VP
  ( Vector )

import qualified Data.Vector.Unboxed.Mutable as WU
  ( MVector )

import qualified Data.Vector.Unboxed as VU
  ( UnboxViaPrim
      ( UnboxViaPrim ) -- Needed for implicit 'Data.Coerce.coerce' when deriving via
  , pattern MV_UnboxViaPrim -- Needed for implicit 'Data.Coerce.coerce' when deriving via
  , pattern V_UnboxViaPrim -- Needed for implicit 'Data.Coerce.coerce' when deriving via
  , Unbox
  , Vector
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


newtype instance forall s. WU.MVector s ValStatus = WUValStatus (WP.MVector s ValStatus)
deriving via (VU.UnboxViaPrim ValStatus) instance W.MVector WU.MVector ValStatus

newtype instance VU.Vector ValStatus = VUValStatus (VP.Vector ValStatus)
deriving via (VU.UnboxViaPrim ValStatus) instance V.Vector VU.Vector ValStatus

instance VU.Unbox ValStatus