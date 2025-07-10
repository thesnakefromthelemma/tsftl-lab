{-# LANGUAGE Haskell2010
  , AllowAmbiguousTypes
  , KindSignatures
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  #-}

{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.Vector where

import Data.Kind
  ( Type )

import Data.Bifunctor
  ( second )

import Data.List
  ( unfoldr
  , scanl'
  )

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
diff = \va ->
    fmap (second $ V.zipWith subtract va) $ V.uncons va

{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given vector of 'Num' values,
assumed to be sequential with increment 1\;
the implementation caches each intermediate derivative
as a generic 'Data.Vector.Generic.Vector' instance,
represented by the ambiguous type variable @v@,
which is __necessarily specialized by type application at the call site__.
-}
{-# INLINE taylor #-}
taylor :: forall (v :: Type -> Type) a.
    (V.Vector v a, Num a) =>
    [a] -> [a]
taylor = unfoldr (diff @v) . V.fromList

{- | The \(n^{\text{th}}\) row of Pascal's triangle
as a list of \(n+1\) 'Integral' values,
where \(n\) is the argument\;
for all but the \(n\) closest to zero,
an arbitrary precision 'Integral' type
should be used!
-}
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\b a -> b * (n - a + 1) `quot` a) 1 [1..]

{- | Extrapolates a given finite list of 'Num' values,
assumed to be sequential from 0 with increment 1,
to a polynomial function of minimal degree
with arguments 'Integral' values\;
the ambiguous type variable @v@, representing
a generic 'Data.Vector.Generic.Vector' instance,
is __necessarily specialized by type application at the call site__.
-}
{-# INLINE extrapolate #-}
extrapolate :: forall (v :: Type -> Type) a n.
    (V.Vector v a, Num a, Integral n) =>
    [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor @v sa) . fmap fromIntegral . pascal