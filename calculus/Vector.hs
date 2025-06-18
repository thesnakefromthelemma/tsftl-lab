{-# LANGUAGE Haskell2010
  , CPP
  , KindSignatures
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  #-}

module Vector where

import Data.Kind
  ( Type )

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

#if EAGER && BOXED
import qualified Data.Vector.Strict as VE
  ( Vector )
#elif EAGER
import qualified Data.Vector.Unboxed as VU
  ( Unbox
  , Vector
  )
#elif BOXED
import qualified Data.Vector as VL
  ( Vector )
#endif

-- * (Discrete) Calculus

-- | Passes on (the knowable initial segment of)
-- the discrete derivative of a given vector of 'Num' values,
-- assumed to be sequential with increment 1,
-- as well as the evaluation of the former at 0,
-- until we know 'Nothing'
{-# INLINE diff #-}
diff :: forall (v :: Type -> Type) a.
    (V.Vector v a, Num a) =>
    v a -> Maybe (a, v a)
diff = \va -> case V.uncons va of
    Just (a, va') -> Just . (a,) $ V.zipWith (-) va' va
    Nothing       -> Nothing

-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given vector of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
#if EAGER && BOXED
taylor :: forall a. Num a => [a] -> [a]
taylor = unfoldr diff . V.fromList @VE.Vector
#elif EAGER
taylor :: forall a. (VU.Unbox a, Num a) => [a] -> [a]
taylor = unfoldr diff . V.fromList @VU.Vector
#elif BOXED
taylor :: forall a. Num a => [a] -> [a]
taylor = unfoldr diff . V.fromList @VL.Vector
#endif

-- | The \(n^{\text{th}}\) row of Pascal's triangle
-- as a list of \(n+1\) 'Integral' values,
-- where \(n\) is the argument
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\b a -> b * (n - a + 1) `quot` a) 1 [1..]

-- | Extrapolates a given finite list of 'Num' values,
-- assumed to be sequential from 0 with increment 1,
-- to a polynomial function of minimal degree
-- with arguments 'Integral' values
{-# INLINE extrapolate #-}
#if BOXED
extrapolate :: forall a n. (Num a, Integral n) => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal
#else
extrapolate :: forall a n. (VU.Unbox a, Num a, Integral n) => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal
#endif