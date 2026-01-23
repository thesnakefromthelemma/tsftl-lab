{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , ScopedTypeVariables
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Strict 'Prelude.Either' -}
module Data.Either
  ( -- * Strict 'Prelude.Either'
    Either
      ( Left
      , Right
      )
  ) where

-- + Imports

-- ++ From base:

import Prelude hiding
  ( Either (..) )

import Data.Kind
  ( Type )


-- * Strict 'Prelude.Maybe'

{- | Strict 'Prelude.Maybe' -}
data Either :: Type -> Type -> Type where
    Left :: forall a b. !a -> Either a b
    Right :: forall a b. !b -> Either a b

deriving stock instance forall a b. (Eq a, Eq b) => Eq (Either a b)
deriving stock instance forall a b. (Show a, Show b) => Show (Either a b)