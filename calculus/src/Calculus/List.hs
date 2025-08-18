{-# LANGUAGE Haskell2010
  , CPP
  , LambdaCase
  , ScopedTypeVariables
  , TupleSections
#-}

{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.List where

import Data.List
  ( unfoldr
  , scanl'
  )

#if EAGER
import Control.DeepSeq
  ( NFData
  , force
  )
#endif


-- * (Discrete) calculus

{- | Passes on (the knowable initial segment of)
the discrete derivative of a given list of 'Num' values,
assumed to be sequential with increment 1,
as well as the evaluation of the former at 0,
until we know 'Nothing'
-}
{-# INLINE diff #-}
diff :: forall a. Num a => [a] -> Maybe (a, [a])
diff = \case
    sa@(a : sa') -> Just . (a,) $ zipWith (-) sa' sa
    []           -> Nothing

{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given list of 'Num' values,
assumed to be sequential with increment 1\;
#if EAGER
implementation caches each intermediate derivative
as a forced list (i.e., 'Data.List.List')
#else
implementation caches each intermediate derivative
as an unforced list (i.e., 'Data.List.List')
#endif
-}
{-# INLINE taylor #-}
#if EAGER
taylor :: forall a. (Num a, NFData a) => [a] -> [a]
taylor = unfoldr $ diff . force
#else
taylor :: forall a. Num a => [a] -> [a]
taylor = unfoldr diff
#endif

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
with arguments 'Integral' values
-}
{-# INLINE extrapolate #-}
#if EAGER
extrapolate :: forall a n. (Num a, Integral n, NFData a) => [a] -> n -> a
extrapolate = \ sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal
#else
extrapolate :: forall a n. (Num a, Integral n) => [a] -> n -> a
extrapolate = \ sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal
#endif