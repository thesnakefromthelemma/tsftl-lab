{-# LANGUAGE Haskell2010
  , CPP
  , LambdaCase
  , ScopedTypeVariables
  , TupleSections
  #-}

module List where

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


-- * (Discrete) Calculus

-- | Passes on (the knowable initial segment of)
-- the discrete derivative of a given list of 'Num' values,
-- assumed to be sequential with increment 1,
-- as well as the evaluation of the former at 0,
-- until we know 'Nothing'
{-# INLINE diff #-}
diff :: forall a. Num a => [a] -> Maybe (a, [a])
diff = \case
    sa@(a : sa') -> Just . (a,) $ zipWith (-) sa' sa
    []           -> Nothing

-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given list of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
#if EAGER
taylor :: forall a. (Num a, NFData a) => [a] -> [a]
taylor = unfoldr $ diff . force
#else
taylor :: forall a. Num a => [a] -> [a]
taylor = unfoldr diff
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
#if EAGER
extrapolate :: forall a n. (Num a, Integral n, NFData a) => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal
#else
extrapolate :: forall a n. (Num a, Integral n) => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal
#endif