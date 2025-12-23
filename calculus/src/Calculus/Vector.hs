{-# LANGUAGE Haskell2010
  , KindSignatures
  , RequiredTypeArguments
  , ScopedTypeVariables
  , TypeApplications
#-}

{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.Vector where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )

import Data.Bifunctor
  ( second )

import Data.List
  ( unfoldr
  , scanl'
  )


-- ++ From vector:

import qualified Data.Vector.Generic as V
  ( Vector
  , fromList
  , zipWith
  , uncons
  )


-- * (Discrete) calculus

{- | Passes on (the knowable initial segment of)
the discrete derivative of a given vector of 'Num' values,
assumed to be sequential with increment 1,
as well as the evaluation of the former at 0,
until we know 'Nothing'
-}
{-# INLINE diff #-}
diff :: forall (v :: Type -> Type) a.
    (V.Vector v a, Num a) =>
    v a -> Maybe (a, v a)
diff = \ va ->
    fmap (second $ V.zipWith subtract va) $ V.uncons va

{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given vector of 'Num' values,
assumed to be sequential with increment 1\;
the implementation caches each intermediate derivative
as a generic 'Data.Vector.Generic.Vector' instance,
represented by the required type argument @v@.
-}
{-# INLINE taylor #-}
taylor :: forall (v :: Type -> Type) ->
    forall a.
    (V.Vector v a, Num a) =>
    [a] -> [a]
taylor = \ v ->
    unfoldr (diff @v) . V.fromList

{- | The \(n^{\text{th}}\) row of Pascal's triangle
as a list of \(n+1\) 'Integral' values,
where \(n\) is the argument\;
for all but the \(n\) closest to zero,
an arbitrary precision 'Integral' type
should be used!
-}
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \ n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\ b a -> b * (n - a + 1) `quot` a) 1 [1..]

{- | Extrapolates a given finite list of 'Num' values,
assumed to be sequential from 0 with increment 1,
to a polynomial function of minimal degree
with arguments 'Integral' values\;
the required type argument @v@ represents
a generic 'Data.Vector.Generic.Vector' instance.

==== __Demo__
>>> :set -XHaskell2010 -XTypeApplications -Wall
>>> import qualified Calculus.Vector ( extrapolate )
>>> import qualified Data.Vector.Unboxed ( Vector )
>>> Calculus.Vector.extrapolate Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] 100
5050
>>> Calculus.Vector.extrapolate Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] 10000
50005000
>>> Calculus.Vector.extrapolate Data.Vector.Unboxed.Vector @Int @Integer [0,1,3,6] (-100)
4950
-}
{-# INLINE extrapolate #-}
extrapolate :: forall (v :: Type -> Type) ->
    forall a n.
    (V.Vector v a, Num a, Integral n) =>
    [a] -> n -> a
extrapolate = \ v sa ->
    sum . zipWith (*) (taylor v sa) . fmap fromIntegral . pascal
