{-# LANGUAGE Haskell2010
  , ScopedTypeVariables
  , TupleSections
  #-}

module Calculus.Vector where

import Data.List
  ( unfoldr
  , scanl'
  )

import qualified Data.Vector.Generic as V
  ( fromList
  , zipWith
  , uncons
  )

import qualified Data.Vector.Strict as VS
  ( Vector )


-- * (Discrete) Calculus

-- | Passes on (the knowable initial segment of)
-- the discrete derivative of a given vector of 'Num' values,
-- assumed to be sequential with increment 1,
-- as well as the evaluation of the former at 0,
-- until we know 'Nothing'
{-# INLINE diff #-}
diff :: forall a. Num a => VS.Vector a -> Maybe (a, VS.Vector a)
diff = \va -> case V.uncons va of
    Just (a, va') -> Just . (a,) $ V.zipWith (-) va' va
    Nothing       -> Nothing

-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given vector of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
taylor :: forall a. Num a => [a] -> [a]
taylor =
    unfoldr diff . V.fromList

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
extrapolate :: forall a n. (Num a, Integral n) => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal