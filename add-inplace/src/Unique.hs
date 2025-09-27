{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , MagicHash
  , PartialTypeSignatures
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  , TypeAbstractions
  , UnboxedTuples
#-}

{-# OPTIONS_GHC
    -Wall
    -Wno-partial-type-signatures
#-}

{- | Unique references (typically to memory)
    via the 'Control.Monad.ST' quantification trick
-}
module Unique
  ( -- * 'Unique#' and 'Frozen#'
    Unique
      ( Unique
      , unUnique
      )
  , Frozen
      ( Frozen
      , unFrozen
      )
  , thawUnsafe
  , freeze
  ) where


-- + Imports

-- ++ From base >= 4.21 && << 4.22

import Prelude hiding
  ( fmap
  , pure
  )

import GHC.Exts
  ( UnliftedType
  , unsafeCoerce#
  , State#
  , RealWorld
  , runRW#
  )

import Data.Kind
  ( Type )


-- * 'Unique'

data Frozen :: (Type -> UnliftedType) -> Type where
    Frozen :: forall (r :: Type -> UnliftedType). {
        unFrozen :: r RealWorld } ->
        Frozen r

newtype Unique :: (Type -> UnliftedType) -> Type where
    Unique :: forall (r :: Type -> UnliftedType). {
        unUnique :: forall s. State# s -> (# State# s, r s #) } ->
        Unique r

{-# INLINE thawUnsafe #-}
thawUnsafe :: forall (r :: Type -> UnliftedType).
    Frozen r -> Unique r
thawUnsafe = \ (Frozen rs) -> Unique $
    \ @s s -> (# s, #) $ (unsafeCoerce# :: r RealWorld -> r s) rs

{-# INLINE freeze #-}
freeze :: forall (r :: Type -> UnliftedType).
    Unique r -> Frozen r
freeze = \ (Unique xrs) -> runRW# $
    \ s -> case xrs s of
        (# _, rs #) -> Frozen $ (unsafeCoerce# :: r s -> r RealWorld) rs