{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Stricter variants of standard "Prelude" types\;
    not apparently necessary (based on profiling)
-}
module Misc
  ( -- * Misc.
    Tup2
      ( Tup2
      , fst
      , snd
      )
  , Maybe
      ( Just 
      , Nothing
      )
  , Either
      ( Left
      , Right
      )
  ) where


-- + Imports

-- ** From base

import Prelude hiding
  ( fst
  , snd
  , Maybe
      ( Just
      , Nothing
      )
  , Either
      ( Left
      , Right
      )
  )

import Data.Kind
  ( Type )


-- * Misc.

data Tup2 :: Type -> Type -> Type where
    Tup2 :: forall a b. {
        fst :: !a ,
        snd :: !b } ->
        Tup2 a b

data Maybe :: Type -> Type where
    Just :: forall a. !a -> Maybe a
    Nothing :: forall a. Maybe a

data Either :: Type -> Type -> Type where
    Left :: forall a b. !a -> Either a b
    Right :: forall a b. !b -> Either a b