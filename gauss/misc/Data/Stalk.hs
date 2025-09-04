{-# LANGUAGE Haskell2010
  , DeriveFoldable
  , DeriveFunctor
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , LambdaCase
  , PartialTypeSignatures
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC
    -Wall
    -Wno-partial-type-signatures
#-}

module Data.Stalk
  ( -- * 'Stalk's
    Stalk
      ( Yield
      , Extend
      )
    -- * Fold/build fusion
  , build
  , fold
    -- * Harvest
  , yield
  ) where


-- * Imports

-- * From base

import Data.Kind
  ( Type )


-- * 'Stalk's

data Stalk :: Type -> Type -> Type where
    Yield :: forall a b. b -> Stalk b a
    Extend :: forall a b. a -> Stalk b a -> Stalk b a

deriving stock instance forall a b. (Eq a, Eq b) => Eq (Stalk b a)
deriving stock instance forall a b. (Show a, Show b) => Show (Stalk b a)
deriving stock instance forall b. Functor (Stalk b)
deriving stock instance forall b. Foldable (Stalk b)


-- * Fold/build fusion for 'Stalk's

{-# INLINE [1] build #-}
build :: forall a b. (forall c. (a -> c -> c) -> (b -> c) -> c) -> Stalk b a
build = \ h ->
    h Extend Yield

{-# INLINE [0] fold #-}
fold :: forall a b c. (a -> c -> c) -> (b -> c) -> Stalk b a -> c
fold = \ g f -> \case
    Yield b       -> f b
    Extend a sba' -> g a $ fold g f sba'

{-# RULES
    "fold/build"
        forall g f (h :: forall c. (_ -> c -> c) -> (_ -> c) -> c).
            fold g f (build h) = h g f
#-}


-- * Harvest

{-# INLINE yield #-}
yield :: forall a b. Stalk b a -> b
yield = fold (const id) id