{-# LANGUAGE Haskell2010
  , AllowAmbiguousTypes
  , GADTSyntax
  , ImpredicativeTypes
  , KindSignatures
  , LambdaCase
  , NoMonoLocalBinds
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.MVector
  ( -- * Lazy 'Data.List.unfoldr' in 'Control.Monad.ST'
    unfoldrST
    -- * (Discrete) calculus
  , diff
  , taylor
  , pascal
  , extrapolate
  ) where

import Data.Kind
  ( Type )

import Control.Monad.ST
  ( ST
  , runST
  )

import Control.Monad.ST.Unsafe
  ( unsafeDupableInterleaveST )

import GHC.Base
  ( build )

import Data.List
  ( scanl' )

import qualified Data.Vector.Generic as V
  ( Vector
  , Mutable
  , fromList
  , thaw
  )

import qualified Data.Vector.Generic.Mutable as W
  ( unsafeWrite
  , unsafeInit
  , length
  , unsafeRead
  )


-- * Lazy 'Data.List.unfoldr' in 'Control.Monad.ST'

{- | The monadic generalization of 'Data.List.unfoldr'
in the case of 'Control.Monad.ST.ST';
seems like it should be standard?

More generally, one could implement

> unfoldrM :: forall (m :: Type -> Type) s a.
>     Monad m => (m [a] -> m [a]) ->
>     (s -> m (Maybe (a, s))) -> s -> m [a]
> unfoldrM = \ f g s ->
>     f $ g s >>= \case
>         Just (a, s') -> (a :) <$> unfoldrM f g s'
>         Nothing      -> pure []

where the argument of type @m [a] -> m [a]@ corresponds
to a \"deferral\" function (prototypically
'Control.Monad.ST.Unsafe.unsafeDupableInterleaveST'),
allowing effects to optionally be performed lazily,
potentially dramatically reducing stack/heap usage
at the expense of the ability to safely consume
the argument of type @s@ elsewhere!

In particular, the below is a safe(?) interface
to @unfoldrM unsafeDupableInterleaveST@,
where the state token of type @s@ is uniquely referenced 
via the rank-2 quantification trick\; na\"ively
we could implement it given the above as just

> unfoldrST = \ g s -> runST $
>     unfoldrM Control.Monad.ST.Unsafe.unsafeDupableInterleaveST g s

But we actually muck around with 'GHC.Base.build' so that
the output (hopefully!) fuses\; experiments
seem to suggest that it indeed does.
-}
{-# INLINE unfoldrST #-}
unfoldrST :: forall (ref :: Type -> Type) a.
    (forall s. ref s -> ST s (Maybe (a, ref s))) -> (forall s. ST s (ref s)) -> [a]
unfoldrST = \ x rs0 ->
    build $ \ g b ->
        let buildR = \ rs -> -- /requires -XNoMonoLocalBinds to typecheck/
                unsafeDupableInterleaveST $ x rs >>= \case
                    Just (a, rs') -> g a <$> buildR rs'
                    Nothing       -> pure b
        in  runST $ buildR =<< rs0


-- * (Discrete) calculus

{- | Passes on (the knowable initial segment of)
the discrete derivative of a given vector of 'Num' values
assumed to be sequential with increment 1,
as well as the evaluation of the former at 0,
until we know 'Nothing'

Computed in place using a generic mutable 'Data.Vector.Generic.Vector'
captured by the 'Control.Monad.ST.ST' monad
-}
{-# INLINE diff #-}
diff :: forall (v :: Type -> Type) a s.
    (V.Vector v a, Num a) =>
    V.Mutable v s a -> ST s (Maybe (a, V.Mutable v s a))
diff = \ wa -> do
    let len' = W.length wa - 1
    case compare 0 len' of
        GT -> pure Nothing
        _  -> do
            a0 <- W.unsafeRead wa 0
            let wa' = W.unsafeInit wa
                diffR = \ i a -> case compare i len' of
                    LT -> do
                        let i' = i + 1
                        a' <- W.unsafeRead wa i'
                        W.unsafeWrite wa' i $ a' - a
                        diffR i' a'
                    _  -> pure wa'
            Just . (a0,) <$> diffR 0 a0

{- | Newtype that fiddles with the order of type arguments
in @forall (v :: Type -> Type) s a. V.Mutable v s a@
so that we can use the safe(?) 'unfoldrST' instead of
directly using the unsafe @unfoldrM unsafeDupableInterleaveST@
-}
newtype WSyn :: (Type -> Type) -> Type -> Type -> Type where
    WSyn :: forall (v :: Type -> Type) a s.
        { unWSyn :: V.Mutable v s a } -> WSyn v a s

{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given vector of 'Num' values
assumed to be sequential with increment 1

The implementation caches each intermediate derivative
as a generic 'Data.Vector.Generic.Vector.Mutable' instance
captured by the 'Control.Monad.ST.ST' monad,
its parametrizing immutable vector type represented
by the ambiguous type variable @v@, which is
__necessarily specialized by type application at the call site__.
-}
{-# INLINE taylor #-}
taylor :: forall (v :: Type -> Type) a.
    (V.Vector v a, Num a) =>
    [a] -> [a]
taylor = unfoldrST (fmap (fmap (fmap WSyn)) . diff @v . unWSyn) . fmap WSyn . V.thaw . V.fromList
    -- /requires -XImpredicativeTypes to typecheck/

{- | The \(n\)th row of Pascal's triangle
as a list of \(n+1\) 'Integral' values,
where \(n\) is the argument

For all but the \(n\) closest to zero,
an arbitrary precision 'Integral' type
should be used!
-}
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \ n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\ b a -> b * (n - a + 1) `quot` a) 1 [1..]

{- | Extrapolates a given finite list of 'Num' values
assumed to be sequential from 0 with increment 1
to a polynomial function of minimal degree
with arguments 'Integral' values

The ambiguous type variable @v@, representing
a generic 'Data.Vector.Generic.Vector' instance,
is __necessarily specialized by type application at the call site__.

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall
>>> import qualified Data.Vector.Unboxed ( Vector )
>>> import qualified Calculus.MVector ( extrapolate )
>>> Calculus.MVector.extrapolate @Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] 100
5050
>>> Calculus.MVector.extrapolate @Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] 10000
50005000
>>> Calculus.MVector.extrapolate @Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] (-100)
4950
-}
{-# INLINE extrapolate #-}
extrapolate :: forall (v :: Type -> Type) a n.
    (V.Vector v a, Num a, Integral n) =>
    [a] -> n -> a
extrapolate = \ sa ->
    sum . zipWith (*) (taylor @v sa) . fmap fromIntegral . pascal