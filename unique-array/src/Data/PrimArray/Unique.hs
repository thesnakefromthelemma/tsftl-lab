{-# LANGUAGE Haskell2010
  , GADTSyntax
  , ImpredicativeTypes
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TupleSections
  , TypeAbstractions
  , TypeApplications
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | A higher-level interface
    to (Mutable)WordArrays as in "GHC.Num.WordArray"
    in the 'Control.Monad.ST.ST' monad 
-}
module Data.PrimArray.Unique
  ( -- * 'MutablePrimArray#'
    new#
  , 
    {- -- * 'MutablePrimArray'
    MutablePrimArray
      ( MutablePrimArray
      , unMutablePrimArray
      )
  -- * 'MutablePrimArray' (re)allocation
  , new
  , resize
  , shrink
  , copy
  -- * 'MutablePrimArray' size information
  , getSize
  -- * 'MutablePrimArray' element access
  , write
  , read
  -- * (Mutable)PrimArray conversion
  , thawUnsafe
  , thaw
  , freeze
  -- * 'PrimArray's
  , PrimArray
      ( PrimArray )
  -- * 'PrimArray' size information
  , size
  -- * 'PrimArray' element access
  , index-}
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.22

import Prelude hiding
  ( read )

import Data.Kind
  ( Type )

import GHC.Exts
  ( unsafeCoerce#
  , RealWorld
  , runRW#
  , pattern I#
  , (*#)
  , pattern W#
  , MutableByteArray#
  , newByteArray#
  , copyMutableByteArrayNonOverlapping#
  , resizeMutableByteArray#
  , shrinkMutableByteArray#
  , getSizeofMutableByteArray#
  , writeWordArray#
  , readWordArray#
  )

import GHC.ST
  ( ST
      ( ST )
  )

import Data.Proxy
  ( pattern Proxy )

-- ++ From primitive >= 0.9.1 && < 0.10

import Data.Primitive.Types
  ( Prim
      ( sizeOfType#
      , writeByteArray#
      , readByteArray#
      )
  )

-- ++ (internal)

import Unique
  ( Frozen
  , Unique
      ( Unique )
  )


-- * 'UniquePrimArray#'

type MutablePrimArray# = MutableByteArray#




-- * 'MutablePrimArray's

data MutablePrimArray :: Type -> Type -> Type where
    MutablePrimArray :: forall a s. {
        unMutablePrimArray :: MutableByteArray# s } ->
        MutablePrimArray a s


-- * 'MutablePrimArray' (re)allocation

{-# INLINE new #-}
new :: forall a s.
    Prim a =>
    Int -> ST s (MutablePrimArray a s)
new = \ (I# n) -> MutablePrimArray . Unique $
    \ s -> newByteArray# ((sizeOfType# @a Proxy) *# n) s

{-# INLINE resize #-}
resize :: forall a.
    Prim a =>
    MutablePrimArray a -> Int -> MutablePrimArray a
resize = \ (MutablePrimArray (Unique xba)) (I# n) -> MutablePrimArray . Unique $
    \ s -> case xba s of
        (# s', ba #) -> resizeMutableByteArray# ba ((sizeOfType# @a Proxy) *# n) s'

{-# INLINE shrink #-}
shrink :: forall a.
    Prim a =>
    MutablePrimArray a -> Int -> MutablePrimArray a
shrink = \ (MutablePrimArray (Unique xba)) (I# n) -> MutablePrimArray . Unique $
    \ s -> case xba s of
        (# s', ba #) -> (# , ba #) $ shrinkMutableByteArray# ba ((sizeOfType# @a Proxy) *# n) s'

{-# INLINE copy #-}
copy :: forall a s.
    Prim a =>
    MutablePrimArray a s -> Int -> MutablePrimArray a s -> Int -> Int -> ST s (MutablePrimArray a s)
copy = undefined


-- * 'MutablePrimArray' size information

{-# INLINE getSize #-}
getSize :: forall a.
    Prim a =>
    MutableWordArray a -> Int
getSize = \ (MutableWordArray (Unique xba)) -> ST $
    \ s -> case getSizeofMutableByteArray# wa s of
        (# s', b #) -> (# s', I# $ bytesToWords# b #)

{-
-- * 'MutablePrimArray' element access

{-# INLINE write #-}
write :: forall s. MutableWordArray s -> Int -> Word -> ST s ()
write = \ (MutableWordArray wa) (I# w) (W# a) -> ST $
    \ s -> case writeWordArray# wa w a s of
        s' -> (# s', () #)

{-# INLINE read #-}
read :: forall s. MutableWordArray s -> Int -> ST s Word
read = \ (MutableWordArray wa) (I# w) -> ST $
    \ s -> case readWordArray# wa w s of
        (# s', a #) -> (# s', W# a #)


-- * 'PrimArray's

newtype MutablePrimArray :: Type -> Type where
    MutablePrimArray :: forall a. {
        unMutablePrimArray :: Unique MutableByteArray# } ->
        MutablePrimArray a

newtype PrimArray :: Type -> Type where
    PrimArray :: forall a. {
        unPrimArray :: Frozen MutableByteArray# } ->
        PrimArray a


-- * 'PrimArray' size information

-- * 'PrimArray' element access
-}