{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Strict 'Maybe' -}
module Data.Maybe.Strict
  ( Maybe
      ( Just
      , Nothing
      )
  ) where

-- + Imports

-- ++ From base:

import Prelude hiding
  ( Maybe
      ( Just
      , Nothing
      )
  )

import Data.Kind
  ( Type )


-- * Strict 'Maybe'

{- | Strict 'Maybe' -}
data Maybe :: Type -> Type where
    Just :: forall a. !a -> Maybe a
    Nothing :: forall a. Maybe a