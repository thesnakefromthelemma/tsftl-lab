{-# LANGUAGE Haskell2010
  , LinearTypes
  , ScopedTypeVariables
  #-}

module Calculus.Linear where


-- * (Discrete) Calculus

-- | (The knowable initial segment of)
-- the discrete derivative of a given list of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE diff #-}
-- diff :: forall a. Num a => undefined a -> undefined a
diff = undefined

-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given list of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
-- taylor :: forall a. Num a => undefined a -> undefined a
taylor = undefined

-- | The \(n^{\text{th}}\) row of Pascal's triangle
-- as a list of \(n+1\) values of type 'Integer'
-- where \(n\) is its argument.
{-# INLINE pascal #-}
-- pascal :: forall n. Integral n => n -> undefined n
pascal = undefined

-- | Extrapolates a given finite list of 'Num' values,
-- assumed to be sequential from 0 with increment 1,
-- to a polynomial function of minimal degree
-- with arguments of type 'Integer'
{-# INLINE extrapolate #-}
extrapolate :: forall a n. (Num a, Integral n) => [a] -> n -> a
extrapolate = undefined