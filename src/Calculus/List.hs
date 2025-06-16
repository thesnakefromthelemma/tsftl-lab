{-# LANGUAGE Haskell2010
  , LambdaCase
  , ScopedTypeVariables
  #-}

module Calculus.List where

import Data.List
  ( scanl' )

import Control.DeepSeq
  ( NFData
  , force
  )


-- * (Discrete) Calculus

-- | (The knowable initial segment of)
-- the discrete derivative of a given list of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE diff #-}
diff :: forall a. Num a => [a] -> [a]
diff = \case
    sa@(_ : sa') -> zipWith (-) sa' sa
    []           -> []

-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given list of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
taylor :: forall a. (Num a, NFData a) => [a] -> [a]
taylor = fmap head . takeWhile (not . null) . iterate (force . diff)

-- | The \(n^{\text{th}}\) row of Pascal's triangle
-- as a list of \(n+1\) values of type 'Integer'
-- where \(n\) is its argument.
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\b a -> b * (n - a + 1) `quot` a) 1 [1..]

-- | Extrapolates a given finite list of 'Num' values,
-- assumed to be sequential from 0 with increment 1,
-- to a polynomial function of minimal degree
-- with arguments of type 'Integer'
{-# INLINE extrapolate #-}
extrapolate :: forall a n. (Num a, Integral n, NFData a) => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal
