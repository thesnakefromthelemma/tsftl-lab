{-# LANGUAGE Haskell2010
  , ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTSyntax
  , ImpredicativeTypes
  , InstanceSigs
  , KindSignatures
  , MultiParamTypeClasses
  , QuantifiedConstraints
  , ScopedTypeVariables
  , TypeFamilyDependencies
  , UndecidableInstances
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Monadic uniqueness types via the 'Control.Monad.ST' quantification trick -}
module Data.Unique
  ( -- * 'Dependent'
    Dependent
    -- * 'Unique'
  , UniqueContext
      ( run )
  , Frozen
  , Unique
      ( Unique
      , unUnique
      )
  , UniqueRef
      ( thawUnsafe
      , thaw
      , dup2
      , freeze
      )
    -- * Functor/Applicative/Monad operations
  , fmap
  , pure
  , apply
  , bind
  ) where


-- + Imports

-- ++ From base >= 4.21 && << 4.22

import Prelude hiding
  ( fmap
  , pure
  )

import Data.Kind
  ( Type
  , Constraint
  )

import qualified Data.Functor as Functor
  ( fmap )

import qualified Control.Applicative as Applicative
  ( pure
  , (<*>)
  )

import qualified Control.Monad as Monad
  ( (=<<) )

import qualified Control.Monad.ST as ST
  ( ST
  , runST
  )


-- * 'Dependent'

class (forall s. c (f s)) => Dependent (c :: (Type -> Type) -> Constraint) (f :: Type -> Type -> Type)  -- requires -XUndeciableInstances

instance forall (c :: (Type -> Type) -> Constraint) (f :: Type -> Type -> Type).
    (forall s. c (f s)) =>
    Dependent c f


-- * 'Unique'

class UniqueContext (m :: Type -> Type -> Type) where
    run :: forall a.
        (forall s. m s a) -> a

instance UniqueContext ST.ST where
    {-# INLINE run #-}
    run :: forall a.
        (forall s. ST.ST s a) -> a
    run = ST.runST

type family Frozen (a :: Type -> Type) = b | b -> a

newtype Unique :: (Type -> Type -> Type) -> (Type -> Type) -> Type where
    Unique :: forall (m :: Type -> Type -> Type) (a :: Type -> Type). {
        unUnique :: forall s. m s (a s) } ->
        Unique m a

class UniqueContext m => UniqueRef (m :: Type -> Type -> Type) (a :: Type -> Type) where
    thawUnsafe :: Frozen a -> Unique m a

    {-# INLINE thaw #-}
    thaw :: Frozen a -> Unique m a
    thaw = undefined

    dup2 :: Unique m a -> (Unique m a, Unique m a)

    freeze :: Unique m a -> Frozen a

    {-# MINIMAL thawUnsafe, dup2, freeze #-}


-- * Functor/Applicative/Monad operations

{-# INLINE fmap #-}
fmap :: forall (f :: Type -> Type -> Type) (a0 :: Type -> Type) (a1 :: Type -> Type).
    Dependent Functor f =>
    (forall s. a0 s -> a1 s) -> Unique f a0 -> Unique f a1
fmap =
    let fmapW :: forall s. (a0 s -> a1 s) -> f s (a0 s) -> f s (a1 s)
        fmapW = Functor.fmap
    in  \ f -> Unique . fmapW f . unUnique

{-# INLINE pure #-}
pure :: forall (f :: Type -> Type -> Type) (a :: Type -> Type).
    Dependent Applicative f =>
    (forall s. a s) -> Unique f a
pure =
    let pureW :: forall s. a s -> f s (a s)
        pureW = Applicative.pure
    in  Unique . pureW

{-# INLINE apply #-}
apply :: forall (f :: Type -> Type -> Type) (a0 :: Type -> Type) (a1 :: Type -> Type).
    Dependent Applicative f =>
    (forall s. f s (a0 s -> a1 s)) -> Unique f a0 -> Unique f a1
apply =
    let applyW :: forall s. f s (a0 s -> a1 s) -> f s (a0 s) -> f s (a1 s)
        applyW = (Applicative.<*>)
    in  \ ff -> Unique . applyW ff . unUnique

{-# INLINE bind #-}
bind :: forall (m :: Type -> Type -> Type) (a0 :: Type -> Type) (a1 :: Type -> Type).
    Dependent Monad m =>
    (forall s. a0 s -> m s (a1 s)) -> Unique m a0 -> Unique m a1
bind =
    let bindW :: forall s. (a0 s -> m s (a1 s)) -> m s (a0 s) -> m s (a1 s)
        bindW = (Monad.=<<)
    in  \ mg -> Unique . bindW mg . unUnique