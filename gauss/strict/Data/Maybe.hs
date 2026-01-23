{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , ScopedTypeVariables
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Strict 'Prelude.Maybe' -}
module Data.Maybe
  ( -- * Strict 'Prelude.Maybe'
    Maybe
      ( Just
      , Nothing
      )
  ) where

-- + Imports

-- ++ From base:

import Prelude hiding
  ( Maybe (..) )

import Data.Kind
  ( Type )


-- * Strict 'Prelude.Maybe'

{- | Strict 'Prelude.Maybe' -}
data Maybe :: Type -> Type where
    Just :: forall a. !a -> Maybe a
    Nothing :: forall a. Maybe a

deriving stock instance forall a. Eq a => Eq (Maybe a)
deriving stock instance forall a. Show a => Show (Maybe a)