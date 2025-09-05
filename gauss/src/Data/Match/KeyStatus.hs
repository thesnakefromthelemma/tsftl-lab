{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , InstanceSigs
  , MagicHash
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

module Data.Match.KeyStatus
  ( -- * KeyStatus
    KeyStatus
      ( KeyStatus
      , name
      , matchCount
      , blockCount
      )
  ) where


-- + Imports

-- ++ base

import GHC.Exts
  ( State#
  , Addr#
  , Int#
  , (+#)
  , (*#)
  , ByteArray#
  , MutableByteArray#
  )

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


-- * KeyStatus

data KeyStatus where
    KeyStatus :: {
        name :: !Int ,
        matchCount :: !Int ,
        blockCount :: !Int } ->
        KeyStatus

deriving stock instance Eq KeyStatus
deriving stock instance Show KeyStatus

instance Ord KeyStatus where
    {-# INLINE compare #-}
    compare :: KeyStatus -> KeyStatus -> Ordering
    compare = \ (KeyStatus n0 n1 n2) (KeyStatus n0' n1' n2') -> case compare (n1 + n2) (n1' + n2') of
        EQ -> case compare n2 n2' of
            EQ -> compare n0 n0'
            o2 -> o2
        o1 -> o1


{- | We could have used 'Data.Vector.Unboxed.As',
    but where's the fun in that?
-}
instance Prim KeyStatus where
    {-# INLINE sizeOfType# #-}
    sizeOfType# ::
        Proxy KeyStatus -> Int#
    sizeOfType# = \ Proxy ->
        3# *# sizeOfType# @Int Proxy

    {-# INLINE alignmentOfType# #-}
    alignmentOfType# ::
        Proxy KeyStatus -> Int#
    alignmentOfType# = \ Proxy ->
        alignmentOfType# @Int Proxy

    {-# INLINE indexByteArray# #-}
    indexByteArray# ::
        ByteArray# -> Int# -> KeyStatus
    indexByteArray# = \ an i ->
        KeyStatus
          ( indexByteArray# an $ 3# *# i )
          ( indexByteArray# an $ 3# *# i +# 1# )
          ( indexByteArray# an $ 3# *# i +# 2# )

    {-# INLINE readByteArray# #-}
    readByteArray# :: forall s.
        MutableByteArray# s -> Int# -> State# s -> (# State# s, KeyStatus #)
    readByteArray# = \ mn i s -> case readByteArray# mn (3# *# i) s of
        (# s', n0 #) -> case readByteArray# mn (3# *# i +# 1#) s' of
            (# s'', n1 #) -> case readByteArray# mn (3# *# i +# 2#) s'' of
                (# s''', n2 #) -> (# s''' , KeyStatus n0 n1 n2 #)

    {-# INLINE writeByteArray# #-}
    writeByteArray# :: forall s.
        MutableByteArray# s -> Int# -> KeyStatus -> State# s -> State# s
    writeByteArray# = \ mn i (KeyStatus n0 n1 n2) s -> case writeByteArray# mn (3# *# i) n0 s of
        s' -> case writeByteArray# mn (3# *# i +# 1#) n1 s' of
            s'' -> writeByteArray# mn (3# *# i +# 2#) n2 s''

    {-# INLINE indexOffAddr# #-}
    indexOffAddr# ::
        Addr# -> Int# -> KeyStatus
    indexOffAddr# = \ x i ->
        KeyStatus
          ( indexOffAddr# x $ 3# *# i )
          ( indexOffAddr# x $ 3# *# i +# 1# )
          ( indexOffAddr# x $ 3# *# i +# 2# )

    {-# INLINE readOffAddr# #-}
    readOffAddr# :: forall s.
        Addr# -> Int# -> State# s -> (# State# s, KeyStatus #)
    readOffAddr# = \ x i s -> case readOffAddr# x (3# *# i) s of
        (# s', n0 #) -> case readOffAddr# x (3# *# i +# 1#) s' of
            (# s'', n1 #) -> case readOffAddr# x (3# *# i +# 2#) s'' of
                (# s''', n2 #) -> (# s''' , KeyStatus n0 n1 n2 #)

    {-# INLINE writeOffAddr# #-}
    writeOffAddr# :: forall s.
        Addr# -> Int# -> KeyStatus -> State# s -> State# s
    writeOffAddr# = \ x i (KeyStatus n0 n1 n2) s -> case writeOffAddr# x (3# *# i) n0 s of
        s' -> case writeOffAddr# x (3# *# i +# 1#) n1 s' of
            s'' -> writeOffAddr# x (3# *# i +# 2#) n2 s''