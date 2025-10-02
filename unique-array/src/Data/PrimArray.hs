{-# LANGUAGE Haskell2010
  , BangPatterns
  , GADTSyntax
  , KindSignatures
  , LambdaCase
  , MagicHash
  , PartialTypeSignatures
  , PatternSynonyms
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  , UnboxedTuples
  , UnliftedNewtypes
#-}

{-# OPTIONS_GHC
    -Wall
    -Wno-partial-type-signatures
#-}

{- | A higher-level interface
    to (Mutable)WordArrays as in "GHC.Num.WordArray"
    in the 'Control.Monad.ST.ST' monad 
-}
module Data.PrimArray
  ( -- * 'MutablePrimArray#'
    -- ** 'MutablePrimArray#'
    MutablePrimArray#
      ( MutablePrimArray#
      , unMutablePrimArray#
      )
    -- ** 'MutablePrimArray#' allocation
  , new#
  , resize#
  , shrink#
  , copy#
  , copyNonOverlapping#
    -- ** 'MutablePrimArray#' from 'Data.List.List'
  , fromList#
  , fromListN#
    -- ** 'MutablePrimArray#' size information
  , getSize#
    -- ** 'MutablePrimArray#' element access
  , write#
  , read#
    -- * 'PrimArray#'
    -- ** 'PrimArray#'
  , PrimArray#
      ( PrimArray#
      , unPrimArray#
      )
    -- ** 'PrimArray#' freezing and thawing
  , thawUnsafe#
  , thaw#
  , freeze#
    -- ** 'PrimArray#' to 'Data.List.List'
  , toList#
    -- ** 'PrimArray#' size information
  , size#
    -- ** 'PrimArray#' element access
  , index#
    -- * 'MutablePrimArray'
    -- ** 'MutablePrimArray'
  , MutablePrimArray
      ( MutablePrimArray
      , unMutablePrimArray
      )
    -- ** 'MutablePrimArray' allocation
  , new
  , resize
  , shrink
  , copy
  , copyNonOverlapping
    -- ** 'MutablePrimArray' from 'Data.List.List'
  , fromList
  , fromListN
    -- ** 'MutablePrimArray' size information
  , getSize
    -- ** 'MutablePrimArray' element access
  , write
  , read
    -- * 'PrimArray'
    -- ** 'PrimArray'
  , PrimArray
      ( PrimArray
      , unPrimArray
      )
    -- ** 'PrimArray' freezing and thawing
  , thawUnsafe
  , thaw
  , freeze
    -- ** 'PrimArray' interconversion with 'Data.List'
  , toList
    -- ** 'PrimArray' size information
  , size
    -- ** 'PrimArray' element access
  , index
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.22

import Prelude hiding
  ( read )

import Data.Kind
  ( Type )

import GHC.Exts
  ( unsafeCoerce#
  , UnliftedType
  , State#
  , RealWorld
  , runRW#
  , Int#
  , pattern I#
  , (+#)
  , (*#)
  , quotInt#
  , (>#)
  , MutableByteArray#
  , newByteArray#
  , copyMutableByteArray#
  , copyMutableByteArrayNonOverlapping#
  , resizeMutableByteArray#
  , shrinkMutableByteArray#
  , getSizeofMutableByteArray#
  )

import GHC.ST
  ( ST
      ( ST )
  )

import Data.Proxy
  ( pattern Proxy )

import qualified GHC.Exts as List
  ( build )

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
  ( Frozen#
      ( Frozen# )
  )


-- * 'MutablePrimArray#'

-- ** 'MutablePrimArray#'

newtype MutablePrimArray# :: Type -> Type -> UnliftedType where
    MutablePrimArray# :: forall a s. {
        unMutablePrimArray# :: MutableByteArray# s } ->
        MutablePrimArray# a s


-- ** 'MutablePrimArray#' allocation

{-# INLINE new# #-}
new# :: forall a s.
    Prim a =>
    Int# -> State# s -> (# State# s, MutablePrimArray# a s #)
new# = \ n s -> case newByteArray# (sizeOfType# @a Proxy *# n) s of
    (# s', bs #) -> (# s', MutablePrimArray# bs #)

{-# INLINE resize# #-}
resize# :: forall a s.
    Prim a =>
    MutablePrimArray# a s -> Int# -> State# s -> (# State# s, MutablePrimArray# a s #)
resize# = \ (MutablePrimArray# bs) n s -> case resizeMutableByteArray# bs (sizeOfType# @a Proxy *# n) s of
    (# s', bs' #) -> (# s', MutablePrimArray# bs' #)

{-# INLINE shrink# #-}
shrink# :: forall a s.
    Prim a =>
    MutablePrimArray# a s -> Int# -> State# s -> State# s
shrink# = \ (MutablePrimArray# bs) n ->
    shrinkMutableByteArray# bs (sizeOfType# @a Proxy *# n)

{-# INLINE copy# #-}
copy# :: forall a s.
    Prim a =>
    MutablePrimArray# a s -> Int# -> MutablePrimArray# a s -> Int# -> Int# -> State# s -> State# s
copy# = \ (MutablePrimArray# bs0) i0 (MutablePrimArray# bs1) i1 n ->
    let i0' = sizeOfType# @a Proxy *# i0
        i1' = sizeOfType# @a Proxy *# i1
        n' = sizeOfType# @a Proxy *# n
    in  copyMutableByteArray# bs0 i0' bs1 i1' n'

{-# INLINE copyNonOverlapping# #-}
copyNonOverlapping# :: forall a s.
    Prim a =>
    MutablePrimArray# a s -> Int# -> MutablePrimArray# a s -> Int# -> Int# -> State# s -> State# s
copyNonOverlapping# = \ (MutablePrimArray# bs0) i0 (MutablePrimArray# bs1) i1 n ->
    let i0' = sizeOfType# @a Proxy *# i0
        i1' = sizeOfType# @a Proxy *# i1
        n' = sizeOfType# @a Proxy *# n
    in  copyMutableByteArrayNonOverlapping# bs0 i0' bs1 i1' n'


-- ** 'MutablePrimArray#' from 'Data.List.List'

{-# INLINE fromList# #-}
fromList# :: forall a s.
    Prim a =>
    [a] -> State# s -> (# State# s, MutablePrimArray# a s #)
fromList# = \case
    [] -> new# 0#
    sa ->
        let -- | Folding function
            fromListF ::
                _ -> _ -> _ -> (# State# s, MutablePrimArray# a s #) -- /Why TF is this necessary?/
            fromListF = \ a k (# s, n, i, pas #) -> case n ># i of
                0# -> let m' = 2# *# n in case resize# pas m' s of
                    (# s', pas' #) -> k (# write# pas' i a s', m', 1# +# i, pas' #)
                _  -> k (# write# pas i a s, n, 1# +# i, pas #)
            -- | Continuation
            fromListK ::
                (# _, Int#, Int#, _ #) -> _ -- /Why TF is this necessary?/
            fromListK = \ (# s, _, n, pas #) ->
                (# shrink# pas n s, pas #)
        in  \ s -> case new# 1# s of
                (# s', pas #) -> foldr fromListF fromListK sa (# s', 1#, 0#, pas #)

{-# INLINE fromListN# #-}
fromListN# :: forall a s.
    Prim a =>
    Int# -> [a] -> State# s -> (# State# s, MutablePrimArray# a s #)
fromListN# = \ n sa ->
    let -- | Folding function
        fromListNF ::
            _ -> _ -> _ -> _ -> (# State# s, MutablePrimArray# a s #) -- /Why TF is this necessary?/
        fromListNF = \ pas a k (# s, i #) ->
            k (# write# pas i a s, 1# +# i #)
        -- | Continuation
        fromListNK :: MutablePrimArray# a s -> (# State# s, Int# #) -> _ -- /Why TF is this necessary?/
        fromListNK = \ pas (# s, _ #) ->
            (# s, pas #)
    in  \ s -> case new# n s of
            (# s', pas #) -> foldr (fromListNF pas) (fromListNK pas) sa (# s', 0# #)


-- ** 'MutablePrimArray#' size information

{-# INLINE getSize# #-}
getSize# :: forall a s.
    Prim a =>
    MutablePrimArray# a s -> State# s -> (# State# s, Int# #)
getSize# = \ (MutablePrimArray# bs) s -> case getSizeofMutableByteArray# bs s of
    (# s', n #) -> (# s', n `quotInt#` sizeOfType# @a Proxy #)


-- ** 'MutablePrimArray#' element access

{-# INLINE write# #-}
write# :: forall a s.
    Prim a =>
    MutablePrimArray# a s -> Int# -> a -> State# s -> State# s
write# = \ (MutablePrimArray# bs) ->
    writeByteArray# bs

{-# INLINE read# #-}
read# :: forall a s.
    Prim a =>
    MutablePrimArray# a s -> Int# -> State# s -> (# State# s, a #)
read# = \ (MutablePrimArray# bs) ->
    readByteArray# bs


-- * 'PrimArray#'

-- ** 'PrimArray#'

newtype PrimArray# :: Type -> UnliftedType where
    PrimArray# :: forall a. {
        unPrimArray# :: Frozen# (MutablePrimArray# a) } ->
        PrimArray# a


-- ** 'PrimArray#' freezing and thawing

{-# INLINE thawUnsafe# #-}
thawUnsafe# :: forall a s.
    Prim a =>
    PrimArray# a -> State# s -> (# State# s, MutablePrimArray# a s #)
thawUnsafe# = \ (PrimArray# (Frozen# (MutablePrimArray# bs))) s ->
    (# s, MutablePrimArray# $ (unsafeCoerce# :: MutableByteArray# RealWorld -> MutableByteArray# s) bs #) -- /A little extra since it's newtypes all the way down/

{-# INLINE thaw# #-}
thaw# :: forall a s.
    Prim a =>
    PrimArray# a -> State# s -> (# State# s, MutablePrimArray# a s #)
thaw# = \ pa s -> case thawUnsafe# pa s of
    (# s', pa' #) -> case getSize# pa' s' of
        (# s'', n #) -> case new# n s'' of
            (# s''', pa'' #) -> (# copyNonOverlapping# pa' 0# pa'' 0# n s''', pa'' #)

{-# INLINE freeze# #-}
freeze# :: forall a.
    Prim a =>
    (forall s. State# s -> (# State# s, MutablePrimArray# a s #)) -> PrimArray# a
freeze# = \ xpas -> runRW# $
    \ s -> case xpas s of
        (# _, pas #) -> PrimArray# $ Frozen# pas


-- ** 'PrimArray#' to 'Data.List.List'

{-# INLINE toList# #-}
toList# :: forall a.
    Prim a =>
    PrimArray# a -> [a]
toList# = \ (PrimArray# (Frozen# pas)) -> List.build $
    \ g b -> runRW# $
        \ s -> -- /Because the order of the reads doesn't matter, we reuse this 'State# RealWorld' token/
            let !(# _, n #) = getSize# pas s
                toListB = \ i -> case n ># i of
                    0# -> b
                    _  -> case read# pas i s of
                        (# _, a #) -> g a (toListB $ 1# +# i)
            in  toListB 0#


-- ** 'PrimArray#' size information

{-# INLINE size# #-}
size# :: forall a.
    Prim a =>
    PrimArray# a -> Int#
size# = \ (PrimArray# (Frozen# pas)) -> runRW# $
    \ s -> case getSize# pas s of
        (# _, n #) -> n


-- ** 'PrimArray#' element access

{-# INLINE index# #-}
index# :: forall a.
    Prim a =>
    PrimArray# a -> Int# -> a
index# = \ (PrimArray# (Frozen# pas)) i -> runRW# $
    \ s -> case read# pas i s of
        (# _, a #) -> a


-- * 'MutablePrimArray'

-- ** 'MutablePrimArray'

data MutablePrimArray :: Type -> Type -> Type where
    MutablePrimArray :: forall a s. {
        unMutablePrimArray :: MutablePrimArray# a s } ->
        MutablePrimArray a s


-- ** 'MutablePrimArray' allocation

{-# INLINE new #-}
new :: forall a s.
    Prim a =>
    Int -> ST s (MutablePrimArray a s)
new = \ (I# n) -> ST $
    \ s -> case new# n s of
        (# s', pas #) -> (# s', MutablePrimArray pas #)

{-# INLINE resize #-}
resize :: forall a s.
    Prim a =>
    MutablePrimArray a s -> Int -> ST s (MutablePrimArray a s)
resize = \ (MutablePrimArray pas) (I# n) -> ST $
    \ s -> case resize# pas n s of
        (# s', pas' #) ->  (# s', MutablePrimArray pas' #)

{-# INLINE shrink #-}
shrink :: forall a s.
    Prim a =>
    MutablePrimArray a s -> Int -> ST s ()
shrink = \ (MutablePrimArray pas) (I# n) -> ST $
    \ s -> (# shrink# pas n s, () #)

{-# INLINE copy #-}
copy :: forall a s.
    Prim a =>
    MutablePrimArray a s -> Int -> MutablePrimArray a s -> Int -> Int -> ST s ()
copy = \ (MutablePrimArray pas0) (I# i0) (MutablePrimArray pas1) (I# i1) (I# n) -> ST $
    \ s -> (# copy# pas0 i0 pas1 i1 n s, () #)

{-# INLINE copyNonOverlapping #-}
copyNonOverlapping :: forall a s.
    Prim a =>
    MutablePrimArray a s -> Int -> MutablePrimArray a s -> Int -> Int -> ST s ()
copyNonOverlapping = \ (MutablePrimArray pas0) (I# i0) (MutablePrimArray pas1) (I# i1) (I# n) -> ST $
    \ s -> (# copyNonOverlapping# pas0 i0 pas1 i1 n s, () #)


-- ** 'MutablePrimArray' from 'Data.List.List'

{-# INLINE fromList #-}
fromList :: forall a s.
    Prim a =>
    [a] -> ST s (MutablePrimArray a s)
fromList = \ sa -> ST $
    \ s -> case fromList# sa s of
        (# s', pas #) -> (# s', MutablePrimArray pas #)

{-# INLINE fromListN #-}
fromListN :: forall a s.
    Prim a =>
    Int -> [a] -> ST s (MutablePrimArray a s)
fromListN = \ (I# n) sa -> ST $
    \ s -> case fromListN# n sa s of
        (# s', pas #) -> (# s', MutablePrimArray pas #)


-- ** 'MutablePrimArray' size information

{-# INLINE getSize #-}
getSize :: forall a s.
    Prim a =>
    MutablePrimArray a s -> ST s Int
getSize = \ (MutablePrimArray pas) -> ST $
    \ s -> case getSize# pas s of
        (# s', n #) -> (# s', I# n #)


-- ** 'MutablePrimArray' element access

{-# INLINE write #-}
write :: forall a s.
    Prim a =>
    MutablePrimArray a s -> Int -> a -> ST s ()
write = \ (MutablePrimArray pas) (I# i) a -> ST $
    \ s -> (# write# pas i a s, () #)

{-# INLINE read #-}
read :: forall a s.
    Prim a =>
    MutablePrimArray a s -> Int -> ST s a
read = \ (MutablePrimArray pas) (I# i) -> ST $
    \ s -> read# pas i s


-- * 'PrimArray'

-- ** 'PrimArray'

data PrimArray :: Type -> Type where
    PrimArray :: forall a. {
        unPrimArray :: PrimArray# a } ->
        PrimArray a


-- ** 'PrimArray' freezing and thawing

{-# INLINE thawUnsafe #-}
thawUnsafe :: forall a s.
    Prim a =>
    PrimArray a -> ST s (MutablePrimArray a s)
thawUnsafe = \ (PrimArray pa) -> ST $
    \ s -> case thawUnsafe# pa s of
        (# s', pas #) -> (# s', MutablePrimArray pas #)

{-# INLINE thaw #-}
thaw :: forall a s.
    Prim a =>
    PrimArray a -> ST s (MutablePrimArray a s)
thaw = \ (PrimArray pa) -> ST $
    \ s -> case thaw# pa s of
        (# s', pas #) -> (# s', MutablePrimArray pas #)

{-# INLINE freeze #-}
freeze :: forall a.
    Prim a =>
    (forall s. ST s (MutablePrimArray a s)) -> PrimArray a
freeze =
    let unwrap = \ (ST xpas) s -> case xpas s of
            (# s', MutablePrimArray pas #) -> (# s', pas #)
    in  \ xpas -> PrimArray $ freeze# $ unwrap xpas


-- ** 'PrimArray' interconversion with 'Data.List'

{-# INLINE toList #-}
toList :: forall a.
    Prim a =>
    PrimArray a -> [a]
toList = \ (PrimArray pa) ->
    toList# pa


-- ** 'PrimArray' size information

{-# INLINE size #-}
size :: forall a.
    Prim a =>
    PrimArray a -> Int
size = \ (PrimArray pa) ->
    I# $ size# pa


-- ** 'PrimArray' element access

{-# INLINE index #-}
index :: forall a.
    Prim a =>
    PrimArray a -> Int -> a
index = \ (PrimArray pa) (I# n) ->
    index# pa n