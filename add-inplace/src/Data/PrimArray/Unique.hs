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
  ( -- * 'MutablePrimArray's
    MutablePrimArray
      ( MutablePrimArray
      , unMutablePrimArray
      )
  -- * 'MutablePrimArray' allocation
  , new
  , dup2Unsafe
  , dup2
  {-, resize
  , shrink
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


-- * 'MutablePrimArray's

newtype MutablePrimArray :: Type -> Type where
    MutablePrimArray :: forall a. {
        unMutablePrimArray :: Unique MutableByteArray# } ->
        MutablePrimArray a


-- * 'MutablePrimArray' allocation

{-# INLINE new #-}
new :: forall a.
    Prim a =>
    Int -> MutablePrimArray a
new = \ (I# n) -> MutablePrimArray . Unique $
    \ s -> case newByteArray# ((sizeOfType# @a Proxy) *# n) s of
        (# s', ba #) -> (# s', ba #)

{-# INLINE dup2Unsafe #-}
dup2Unsafe :: forall a.
    Prim a =>
    MutablePrimArray a -> (MutablePrimArray a, MutablePrimArray a)
dup2Unsafe = \ (MutablePrimArray (Unique xba)) ->
    (\ ~(b0, b1) -> (b0, b1)) $ runRW# $
        \ s -> case xba s of
            (# _, ba #) ->
                ( MutablePrimArray . Unique $ \ @s' s' -> (# s', #) $ (unsafeCoerce# :: MutableByteArray# RealWorld -> MutableByteArray# s') ba
                , MutablePrimArray . Unique $ \ @s' s' -> (# s', #) $ (unsafeCoerce# :: MutableByteArray# RealWorld -> MutableByteArray# s') ba
                )

{-# INLINE dup2 #-}
dup2 :: forall a.
    Prim a =>
    MutablePrimArray a -> Int -> Int -> (MutablePrimArray a, MutablePrimArray a)
dup2 = \ (MutablePrimArray (Unique xba)) (I# t) (I# n) ->
    (\ ~(b0, b1) -> (b0, b1)) $ runRW# $
        let t' = (sizeOfType# @a Proxy) *# t
            n' = (sizeOfType# @a Proxy) *# n
        in  \ s -> case xba s of
                (# s', ba #) -> case newByteArray# n' s' of
                    (# s'', ba' #) -> case copyMutableByteArrayNonOverlapping# ba t' ba' 0# n' s'' of
                        _ ->
                            ( MutablePrimArray . Unique $ \ @s''' s''' -> (# s''', #) $ (unsafeCoerce# :: MutableByteArray# RealWorld -> MutableByteArray# s''') ba
                            , MutablePrimArray . Unique $ \ @s''' s''' -> (# s''', #) $ (unsafeCoerce# :: MutableByteArray# RealWorld -> MutableByteArray# s''') ba'
                            )


{-
{-# INLINE resize #-}
resize :: forall a.
    Prim a =>
    MutablePrimArray a -> Int -> MutablePrimArray a
resize = \ (MutableWordArray wa) (I# w) -> ST $
    \ s -> case resizeMutableByteArray# wa (wordsToBytes# w) s of
        (# s', wa' #) -> (# s', MutableWordArray wa' #) 


-- * 'MutablePrimArray' size information

{-# INLINE size #-}
size :: forall s. MutableWordArray s -> ST s Int
size = \ (MutableWordArray wa) -> ST $
    \ s -> case getSizeofMutableByteArray# wa s of
        (# s', b #) -> (# s', I# $ bytesToWords# b #)


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

newtype PrimArray :: Type -> Type where
    PrimArray :: forall a. {
        unPrimArray :: Frozen MutableByteArray# } ->
        PrimArray a


-- * 'PrimArray' size information

-- * 'PrimArray' element access
-}