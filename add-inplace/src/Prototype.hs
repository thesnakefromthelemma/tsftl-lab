{-# LANGUAGE Haskell2010
  , BangPatterns
  , GADTSyntax
  , KindSignatures
  , LambdaCase
  , MagicHash
  , PatternSynonyms
  , RankNTypes
  , ScopedTypeVariables
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

module Prototype
  ( -- * 'InplaceNatural'
    InplaceNatural
      ( InplaceNatural
      , unInplaceNatural
      , size
      )
  , capacity
  , new
  , resize
    -- * Operations
  , fromWord
  , thaw
  , thawNatural
  , write
  , read
  , run
  , runNatural
  , add
  ) where


-- + Imports

import Prelude hiding
  ( read )

import Data.Kind
  ( Type )

import GHC.Exts
  ( pattern I#
  , int2Word#
  , addWordC#
  , pattern W#
  , isTrue#
  )

import Control.Monad.Primitive
  ( PrimMonad
  , PrimState
  )

import Control.Monad.ST
  ( ST
  , runST
  )

import Data.Primitive.PrimArray
  ( MutablePrimArray
  , newPrimArray
  , writePrimArray
  , getSizeofMutablePrimArray
  , readPrimArray
  , thawPrimArray
  , unsafeFreezePrimArray
  , pattern PrimArray
  )

import GHC.Num.BigNat
  ( BigNat
      ( BN# )
  , bigNatFromWordArray#
  , bigNatSize#
  )

import GHC.Num.Natural
  ( Natural   
  , pattern NS
  , pattern NB
  , naturalFromBigNat#
  )


-- _ Misc. util

{-# INLINE bool2Word #-}
bool2Word :: Bool -> Word
bool2Word = \case
    True  -> 1
    False -> 0

{-# INLINE addWordC #-}
addWordC :: Word -> Word -> (Word, Bool)
addWordC = \ (W# u0) (W# u1) -> case addWordC# u0 u1 of
    (# u', c #) -> (W# u', isTrue# c)

{-# INLINE addWordCC #-}
addWordCC :: Word -> Word -> Bool -> (Word, Bool)
addWordCC = \ a0 a1 c ->
    let (a', c') = addWordC a0 a1
        (a'', c'') = addWordC a' $ bool2Word c
    in  (a'', c' || c'')


-- * 'InplaceNatural'

data InplaceNatural :: Type -> Type where
    InplaceNatural :: forall s. {
        unInplaceNatural :: {-# UNPACK #-} !(MutablePrimArray s Word) ,
        size :: !Int } ->
        InplaceNatural s

{-# INLINE capacity #-}
capacity :: forall (m :: Type -> Type).
    PrimMonad m =>
    InplaceNatural (PrimState m) -> m Int
capacity = \ (InplaceNatural ma _) ->
    getSizeofMutablePrimArray ma

{-# INLINE growthConstant #-}
growthConstant :: Int
growthConstant = 2

{-# INLINE capacityFor #-}
capacityFor :: Int -> Int
capacityFor = \ n ->
    let capacityForR = \ k -> case compare n k of
            GT -> capacityForR $ growthConstant * k
            _  -> k
    in  capacityForR 1

{-# INLINE new #-}
new :: forall (m :: Type -> Type).
    PrimMonad m =>
    Int -> m (InplaceNatural (PrimState m))
new = (flip InplaceNatural 0 <$>) . newPrimArray . capacityFor

{-# INLINE resize #-}
resize :: forall s.
    InplaceNatural s -> Int -> InplaceNatural s
resize = \ (InplaceNatural ma _) n' ->
    InplaceNatural ma n'


-- * Operations

{-# INLINE fromWord #-}
fromWord :: forall (m :: Type -> Type).
    PrimMonad m =>
    Word -> m (InplaceNatural (PrimState m))
fromWord = \ a ->
    let n = case a of
            0 -> 0
            _ -> 1
    in  do  ma <- newPrimArray 1 
            writePrimArray ma 0 a
            pure $ InplaceNatural ma n

{-# INLINE thaw #-}
thaw :: forall (m :: Type -> Type).
    PrimMonad m =>
    BigNat -> m (InplaceNatural (PrimState m))
thaw = \ (BN# vu) ->
    let n = I# $ bigNatSize# vu
    in  flip InplaceNatural n <$> thawPrimArray (PrimArray vu) 0 n

{-# INLINE thawNatural #-}
thawNatural :: forall (m :: Type -> Type).
    PrimMonad m =>
    Natural -> m (InplaceNatural (PrimState m))
thawNatural = \case
    NS u  -> fromWord $ W# u
    NB vu -> thaw (BN# vu)

{-# INLINE write #-}
write :: forall (m :: Type -> Type).
    PrimMonad m =>
    InplaceNatural (PrimState m) -> Int -> Word -> m () 
write = \ (InplaceNatural ma _) i a ->
    writePrimArray ma i a

{-# INLINE read #-}
read :: forall (m :: Type -> Type).
    PrimMonad m =>
    InplaceNatural (PrimState m) -> Int -> m Word
read = \ (InplaceNatural ma _) i ->
    readPrimArray ma i

{-# INLINE run #-}
run :: (forall s. ST s (InplaceNatural s)) -> BigNat
run = \ stx -> runST $ do
    InplaceNatural ma (I# n) <- stx
    PrimArray va <- unsafeFreezePrimArray ma
    pure $ BN# $ bigNatFromWordArray# va $ int2Word# n

{-# INLINE runNatural #-}
runNatural :: (forall s. ST s (InplaceNatural s)) -> Natural
runNatural = \ stx ->
    let !(BN# vu) = run stx
    in  naturalFromBigNat# vu

{-# INLINE add #-}
add :: forall (m :: Type -> Type).
    PrimMonad m =>
    InplaceNatural (PrimState m) -> InplaceNatural (PrimState m) -> m (InplaceNatural (PrimState m))
add = \ x0 x1 -> do
    let n0 = size x0
        n1 = size x1
        (xmax, xmin, nmax, nmin) = case compare n0 n1 of
            LT -> (x1, x0, n1, n0)
            _  -> (x0, x1, n0, n1)
    k <- capacity xmax
    case compare k nmax of
        GT -> do
            let addR = \ i -> case compare nmin i of
                    GT -> \ c -> do
                        amax <- read xmax i
                        amin <- read xmin i
                        let (anew, c') = addWordCC amax amin c
                        write xmax i anew
                        addR (i + 1) c'
                    _  -> \case
                        True  -> overflowR i
                        False -> pure False
                overflowR = \ i -> case compare nmax i of
                    GT -> do
                        amax <- read xmax i
                        let (anew, c') = addWordC amax 1
                        write xmax i anew
                        case c' of
                            True  -> overflowR $ i + 1
                            False -> pure False
                    _   -> pure True
            addR 0 False >>= \case
                True  -> do
                    write xmax nmax 1
                    pure . resize xmax $ nmax + 1
                False -> pure xmax
        _  -> do
            xnew <- (flip resize nmax <$>) . new $ nmax + 1
            let addR = \ i -> case compare nmin i of   
                    GT -> \ c -> do
                        amax <- read xmax i
                        amin <- read xmin i
                        let (anew, c') = addWordCC amax amin c
                        write xnew i anew
                        addR (i + 1) c'
                    _  -> \case
                        True  -> overflowR i
                        False -> copyR i
                overflowR = \ i -> case compare nmax i of
                    GT -> do
                        amax <- read xmax i
                        let (anew, c') = addWordC amax 1
                        write xnew i anew
                        case c' of
                            True  -> overflowR $ i + 1
                            False -> copyR $ i + 1
                    _  -> pure True
                copyR = \ i -> case compare nmax i of
                    GT -> do
                        amax <- read xmax i
                        write xnew i amax
                        copyR $ i + 1
                    _  -> pure False
            addR 0 False >>= \case
                True  -> do
                    write xnew nmax 1
                    pure . resize xnew $ nmax + 1
                False -> pure xnew
