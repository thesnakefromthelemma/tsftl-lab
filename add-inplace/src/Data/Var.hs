{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , MagicHash
  , ScopedTypeVariables
  , TupleSections
  , UnboxedTuples
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Toy model for how 'Data.Unique.Unique' works -}
module Data.Var
  ( -- * 'MutVar'
    MutVar
  , newMutVar
  , writeMutVar
  , readMutVar
    -- * 'Var'
  , Var
  , newVar
  , readVar
    -- * thaw/freeze
  , thawUnsafe
  , freeze
  ) where


-- + Imports

-- ++ From base >= 4.21 && << 4.22

import Data.Kind
  ( Type )

import GHC.Exts
  ( unsafeCoerce#
  , RealWorld
  , runRW#
  , MutVar#
  , newMutVar#
  , writeMutVar#
  , readMutVar#
  )

import GHC.ST
  ( ST
      ( ST )
  )


-- * 'MutVar'

data MutVar :: Type -> Type -> Type where
    MutVar :: forall a s.
        MutVar# s a ->
        MutVar a s

{-# INLINE newMutVar #-}
newMutVar :: forall a s.
    a -> ST s (MutVar a s)
newMutVar = \ a -> ST $
    \ s -> case newMutVar# a s of
        (# s', r #) -> (# s', MutVar r #)

{-# INLINE writeMutVar #-}
writeMutVar :: forall a s.
    MutVar a s -> a -> ST s ()
writeMutVar = \ (MutVar r) a -> ST $
    \ s -> case writeMutVar# r a s of
        s' -> (# s', () #)

{-# INLINE readMutVar #-}
readMutVar :: forall a s.
    MutVar a s -> ST s a
readMutVar = \ (MutVar r) -> ST $
    \ s -> readMutVar# r s


-- * 'Var'

data Var :: Type -> Type where
    Var :: forall a.
        MutVar# RealWorld a ->
        Var a

{-# INLINE newVar #-}
newVar :: forall a.
    a -> Var a
newVar = \ a -> runRW# $
    \ s -> case newMutVar# a s of
        (# _, va #) -> Var va


{-# INLINE readVar #-}
readVar :: forall a.
    Var a -> a
readVar = \ (Var va) -> runRW# $
    \ s -> case readMutVar# va s of
        (# _, a #) -> a


-- * thaw/freeze

{-# INLINE thawUnsafe #-}
thawUnsafe :: forall a s.
    Var a -> ST s (MutVar a s)
thawUnsafe = \ (Var va) -> ST $
    \ s -> (# s, #) $ MutVar $
        (unsafeCoerce# :: MutVar# RealWorld a -> MutVar# s a) va

{-# INLINE freeze #-}
freeze :: forall a s.
    MutVar a s -> ST s (Var a)
freeze = \ (MutVar wa) -> ST $
    \ s -> (# s, #) $ Var $
        (unsafeCoerce# :: MutVar# s a -> MutVar# RealWorld a) wa