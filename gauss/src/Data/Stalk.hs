{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , InstanceSigs
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
    equipped with the corresponding "fold/build" fusion system\;
    we implement more or less the same basic rewrite rules
    as written for 'Data.List.List's in "GHC.Internal.Base"
    and ensure that the two "fold/build" fusion systems
    fuse seamlessly with one another, but do not (yet?) bother
    rewriting fusible forms to simpler ones
    in the final stages of optimization should fusion not occur.
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
  , fromList
  , enchain
  , toList
  , harvest
  ) where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )

import qualified GHC.List as List
  ( build )

import Data.Bifunctor
  ( Bifunctor
      ( bimap )
  )


-- * 'Stalk's

{- | Morally, @'Stalk' b a :=  b a -> Fix (\\ x -> 'Either' b (a, x))@ -}
data Stalk :: Type -> Type -> Type where
    Yield :: forall a b.
        b -> Stalk b a
    Extend :: forall a b.
        a -> Stalk b a -> Stalk b a

deriving stock instance forall a b. (Eq a, Eq b) => Eq (Stalk b a)
deriving stock instance forall a b. (Show a, Show b) => Show (Stalk b a)


-- * "fold/build" fusion for 'Stalk's

{- | The "build" of 'Stalk'\'s "fold/build" fusion system -}
{-# INLINE [0] build #-}
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
"fold/cons/build"
    forall g f a (h :: forall c. (_ -> c -> c) -> (_ -> c) -> c).
        fold g f (Extend a (build h)) = g a (h g f)
"fold/id"
    forall .
        fold Extend Yield = \ sba -> sba
"fold/nil"
    forall g f b.
        fold g f (Yield b)  = f b
"fold/single"
    forall g f a b.
        fold g f (Extend a (Yield b))  = g a (f b)
#-}


-- * Handling 'Stalks'

instance forall b. Functor (Stalk b) where
    {-# INLINE fmap #-}
    fmap :: forall a0 a1.
        (a0 -> a1) -> Stalk b a0 -> Stalk b a1
    fmap = bimap id

instance Bifunctor Stalk where
    {-# INLINE bimap #-}
    bimap :: forall a0 a1 b0 b1.
        (b0 -> b1) -> (a0 -> a1) -> Stalk b0 a0 -> Stalk b1 a1
    bimap = \ y x sb0a0 -> build $
        \ g f -> fold (g . x) (f . y) sb0a0

{-_ If you really want to 'foldr' a @Stalk _ _@, 'toList' it first!
    >>> instance forall b. Foldable (Stalk b) where
    >>>     {-# INLINE foldr #-}
    >>>     foldr :: forall a c. (a -> c -> c) -> c -> Stalk b a -> c
    >>>     foldr = \ g c -> fold g (const c)
-}

{- | For all @a@, @b@,
    constructs a @Stalk b a@ value from
    a 'Data.List.List' of type @a@ and
    a value of type @b@ in the obvious way\;
    'Stalk' fold/build fusible on the left and
    'Data.List.List' fold/build fusible on the right
-}
fromList :: forall a b.
    [a] -> b -> Stalk b a
fromList = \ sa b -> build $
    \ g f -> foldr g (f b) sa

{-| (Dependent) concatenation of 'Stalk's\;
    fold/build fusible via essentially the same
    mechanism achieved by 'GHC.Internal.Base'
    if I'm understanding correctly
-}
{-# INLINE enchain #-}
enchain :: forall a b0 b1.
    Stalk b0 a -> (b0 -> Stalk b1 a) -> Stalk b1 a
enchain = \ sb0a z -> build $
    \ g f -> fold g (fold g f . z) sb0a

{- | For all @a@, @a@,
    collapses a @Stalk b a@ value to
    a @[a]@ value in the obvious way\;
    'Data.List.List' fold/build fusible on the left and
    'Stalk' fold/build fusible on the right
-}
{-# INLINE toList #-}
toList :: forall a b.
    Stalk b a -> [a]
toList = \ sba -> List.build $
    \ g c -> fold g (const c) sba

{- | Extracts the innermost 'Yield' of a value of type @'Stalk' _ _@\;
    nonterminating on infinite 'Stalk's
-}
{-# INLINE harvest #-}
harvest :: forall a b. Stalk b a -> b
harvest = fold (const id) id