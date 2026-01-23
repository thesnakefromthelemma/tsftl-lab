{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , ScopedTypeVariables
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Strict 'Prelude.(,*)' -}
module Data.Tuple
  ( -- * Strict 'Prelude.(,)'
    Tup2
      ( Tup2
      , of2entry0
      , of2entry1
      )
    -- * Strict 'Prelude.(,,,)'
  , Tup4
      ( Tup4
      , of4entry0
      , of4entry1
      , of4entry2
      , of4entry3
      )
  ) where

-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )


-- * Strict


-- * Strict 'Prelude.(,)'

{- | Strict 'Prelude.(,)' -}
data Tup2 :: Type -> Type -> Type where
    Tup2 :: forall a b. {
        of2entry0 :: !a ,
        of2entry1 :: !b } ->
        Tup2 a b

deriving stock instance forall a0 a1. (Eq a0, Eq a1) => Eq (Tup2 a0 a1)
deriving stock instance forall a0 a1. (Show a0, Show a1) => Show (Tup2 a0 a1)


-- * Strict 'Prelude.(,,,)'

{- | Strict 'Prelude.(,,,)' -}
data Tup4 :: Type -> Type -> Type -> Type -> Type where
    Tup4 :: forall a b c d. {
        of4entry0 :: !a ,
        of4entry1 :: !b ,
        of4entry2 :: !c ,
        of4entry3 :: !d } ->
        Tup4 a b c d