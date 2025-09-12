{-# LANGUAGE Haskell2010
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

{- | The bifunctor @\ b a -> Fix (\\ x -> 'Either' b (a, x))@,
    equipped with the corresponding "fold/build" fusion system
-}
module Data.Stalk
  ( -- * 'Stalk's
    Stalk
      ( Yield
      , Extend
      )
    -- * "fold/build" fusion for 'Stalk's
  , build
  , fold
    -- * Handling 'Stalk's
  , harvest
  ) where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )


-- * 'Stalk's

{- | Morally, @'Stalk' b a :=  b a -> Fix (\\ x -> 'Either' b (a, x))@ -}
data Stalk :: Type -> Type -> Type where
    Yield :: forall a b. b -> Stalk b a
    Extend :: forall a b. a -> Stalk b a -> Stalk b a

deriving stock instance forall a b. (Eq a, Eq b) => Eq (Stalk b a)
deriving stock instance forall a b. (Show a, Show b) => Show (Stalk b a)


-- * "fold/build" fusion for 'Stalk's

{- | The "build" of 'Stalk'\'s "fold/build" fusion system -}
{-# INLINE [1] build #-}
build :: forall a b. (forall c. (a -> c -> c) -> (b -> c) -> c) -> Stalk b a
build = \ h ->
    h Extend Yield

{- | The "fold" of 'Stalk'\'s "fold/build" fusion system -}
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


-- * Handling 'Stalks'

{- | Extracts the innermost 'Yield' of a value of type @'Stalk' _ _@\;
    nonterminating on infinite 'Stalk's
-}
{-# INLINE harvest #-}
harvest :: forall a b. Stalk b a -> b
harvest = fold (const id) id