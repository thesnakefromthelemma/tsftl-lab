{-# LANGUAGE Haskell2010
  , GADTSyntax
  , ImpredicativeTypes
  , InstanceSigs
{-, KindSignatures (redundant) -}
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , ScopedTypeVariables
  , TypeFamilies
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | A higher-level interface
    to (Mutable)WordArrays as in "GHC.Num.WordArray"
    in the 'Control.Monad.ST.ST' monad 
-}
module Data.WordArray
  ( -- * (Mutable)WordArrays
    MutableWordArray
      ( MutableWordArray )
  , WordArray
      ( WordArray )
  -- * Allocation
  , new
  , resize
  , copy
  -- * Information
  , size
  -- * Element access
  , write
  , read
  -- * Conversion
  , thawUnsafe
  , thaw
  , freezeUnsafe
  , freeze
  -- * As 'Unique'
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.22

import Prelude hiding
  ( read )

import Data.Kind
  ( Type )

import GHC.Exts
  ( pattern I#
  , pattern W#
  , MutableByteArray#
  , ByteArray#
  , newByteArray#
  , copyMutableByteArrayNonOverlapping#
  , resizeMutableByteArray#
  , getSizeofMutableByteArray#
  , writeWordArray#
  , readWordArray#
  , unsafeThawByteArray#
  , unsafeFreezeByteArray#
  )

import Control.Monad
  ( (<=<) )

import GHC.ST
  ( ST
      ( ST )
  )

-- ++ From ghc-bignum >= 1.1 && < 1.4

import GHC.Num.WordArray
  ( bytesToWords#
  , wordsToBytes#
  )

-- ++ (internal)

import Data.Unique
  ( Frozen
  , Unique
      ( Unique
      , unUnique
      )
  , UniqueRef
  )

import qualified Data.Unique as Unique
  ( run
  , thawUnsafe
  , copy
  , freeze
  )

import qualified Data.Unique as Unique.Monad
  ( bind )


-- * (Mutable)WordArrays

type MutableWordArray# = MutableByteArray#

data MutableWordArray :: Type -> Type where
    MutableWordArray :: forall s.
        MutableWordArray# s ->
        MutableWordArray s

type WordArray# = ByteArray#

data WordArray :: Type where
    WordArray ::
        WordArray# ->
        WordArray


-- * Allocation

{-# INLINE new #-}
new :: forall s. Int -> ST s (MutableWordArray s)
new = \ (I# w) -> ST $
    \ s -> case newByteArray# (wordsToBytes# w) s of
        (# s', wa #) -> (# s', MutableWordArray wa #)

{-# INLINE resize #-}
resize :: forall s. MutableWordArray s -> Int -> ST s (MutableWordArray s)
resize = \ (MutableWordArray wa) (I# w) -> ST $
    \ s -> case resizeMutableByteArray# wa (wordsToBytes# w) s of
        (# s', wa' #) -> (# s', MutableWordArray wa' #) 

{-# INLINE copy #-}
copy :: forall s. MutableWordArray s -> ST s (MutableWordArray s)
copy = \ (MutableWordArray wa) -> ST $
    \ s -> case getSizeofMutableByteArray# wa s of
        (# s', b #) -> case newByteArray# b s' of
            (# s'', wa' #) -> case copyMutableByteArrayNonOverlapping# wa 0# wa' 0# b s'' of
                s''' -> (# s''', MutableWordArray wa' #)


-- * Information

{-# INLINE size #-}
size :: forall s. MutableWordArray s -> ST s Int
size = \ (MutableWordArray wa) -> ST $
    \ s -> case getSizeofMutableByteArray# wa s of
        (# s', b #) -> (# s', I# $ bytesToWords# b #)


-- * Element access

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


-- * Conversion

{-# INLINE thawUnsafe #-}
thawUnsafe :: forall s. WordArray -> ST s (MutableWordArray s)
thawUnsafe = \ (WordArray va) -> ST $
    \ s -> case unsafeThawByteArray# va s of
        (# s', wa #) -> (# s', MutableWordArray wa #)

{-# INLINE thaw #-}
thaw :: forall s. WordArray -> ST s (MutableWordArray s)
thaw = copy <=< thawUnsafe

{-# INLINE freezeUnsafe #-}
freezeUnsafe :: forall s. MutableWordArray s -> ST s WordArray
freezeUnsafe = \ (MutableWordArray wa) -> ST $
    \ s -> case unsafeFreezeByteArray# wa s of
        (# s', va #) -> (# s', WordArray va #)

{-# INLINE freeze #-}
freeze :: forall s. MutableWordArray s -> ST s WordArray
freeze = freezeUnsafe <=< copy


-- * As 'Unique'

type instance Frozen MutableWordArray = WordArray

instance UniqueRef ST MutableWordArray where
    {-# INLINE thawUnsafe #-}
    thawUnsafe :: WordArray -> Unique ST MutableWordArray
    thawUnsafe = Unique . thawUnsafe

    {-# INLINE copy #-}
    copy :: Unique ST MutableWordArray -> Unique ST MutableWordArray
    copy = Unique.Monad.bind copy

    {-# INLINE freeze #-}
    freeze :: Unique ST MutableWordArray -> WordArray
    freeze = Unique.run . (=<<) freezeUnsafe . unUnique