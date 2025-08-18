{-# LANGUAGE Haskell2010
  , BangPatterns
  , GADTs
  , InstanceSigs
  , KindSignatures
  , LambdaCase
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Cell-strict infinite 'Stream' type -}
module Data.Stream.Strict
  ( -- * Helper types
    Nat
  , Pair
      ( Pair )
    -- * Cell-strict infinite 'Stream' type
  , Stream
      ( Cons )
    -- * Basic operations on 'Stream's
  , unfolds'
  , zip
  , scanl'
  , iterate'
  , toStream
  , (!!)
    -- Stream of naturals
  , sNat
  ) where

import Prelude hiding
  ( (!!)
  , zip
  )

import Data.Kind
  ( Type )

-- * Helper types

{- | Natural numbers represented as 'Prelude.Word's -}
type Nat = Word

{- | Eager 'Pair' type -}
data Pair :: Type -> Type -> Type where
    Pair :: forall a b. !a -> !b -> Pair a b


-- * Cell-strict infinite 'Stream' type

{- | Cell-strict infinite 'Stream' type\;
unique constructor
-}
data Stream :: Type -> Type where
    Cons :: forall a. !a -> Stream a -> Stream a

{- | This instance's 'fmap'
behaves as that of infinite 'Data.List.List's\,
except that the constructors of its output
are produced "for free" (as opposed to
by matching strictly onto the constructors
of the second argument of 'fmap')\;
if it is desirable that the cells of the second argument
be evaluated eagerly, then the first argument of 'fmap'
should eagerly evaluate its own argument.
-}
instance Functor Stream where
    fmap :: forall a b. (a -> b) -> Stream a -> Stream b
    fmap = \ f ~(Cons a sa') ->
        Cons (f a) $ fmap f sa'

{- | (Just for convenience, i.e., so that
we have a 'Data.Foldable.toList'
with which to manipulate 'Stream's as 'Data.List.List's\;
same strictness story as that of 'fmap' above)
-}
instance Foldable Stream where
    foldMap :: forall a m. Monoid m => (a -> m) -> Stream a -> m
    foldMap = \ f ~(Cons a sa') ->
        f a <> foldMap f sa'


-- * Basic operations on 'Stream's

{- | Analogue of 'Data.List.unfoldr'
for 'Stream's\; no termination, hence no 'Maybe' -}
unfolds' :: forall s a. (s -> (a, s)) -> s -> Stream a
unfolds' = \ g !s -> case g s of -- /MILU not viable as in general @s'@ might depend on the @Stream a _@ constructor/
    (a, s') -> Cons a $ unfolds' g s'

{- | Analogue of 'Data.List.zip' -}
zip :: forall a b. Stream a -> Stream b -> Stream (Pair a b)
zip = curry $
    unfolds' (\ (~(Cons a sa'), ~(Cons b sb')) -> (Pair a b, (sa', sb')))

{- | Analogue of 'Data.List.scanl''\;
same strictness story as that of 'fmap' above)
-}
scanl' :: forall a b. (b -> a -> b) -> b -> Stream a -> Stream b
scanl' = \ g -> flip . curry $ -- /MILU not viable for same reason as above/
    unfolds' (\ (~(Cons a sa'), !b) -> (b, (sa', g b a)))

{- | Analogue of 'Data.List.iterate''\;
same strictness story as that of 'fmap' above)
-}
iterate' :: forall a. (a -> a) -> a -> Stream a
iterate' = \ f ->
      unfolds' (\ a -> (a, f a))

{- | Stream of natural numbers -}
sNat :: Stream Nat
sNat = iterate' (1 +) 0

{- | Flattens an infinite 'Foldable' type
to a 'Stream'\; not total!
-}
toStream :: forall (f :: Type -> Type) a.
    Foldable f => f a -> Stream a
toStream = foldr Cons undefined

{- | Indexing function into 'Stream's\;
total, unlike '(Prelude.!!)'
-}
infixl 9 !!
(!!) :: forall a. Stream a -> Nat -> a
(!!) = flip $ \cases
    0 (Cons a _  ) -> a
    n (Cons _ sa') -> sa' !! (n - 1)