{-# LANGUAGE Haskell2010
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Numerical integration via Simpson's rule -}
module Math.Calculus.Simpson
  ( -- * Numerical integration via Simpson's rule
    integrate
  ) where

import Data.List
  ( iterate' )

-- * Numerical integration via Simpson's rule

{- | Numerical integration via Simpson's rule\;
neither fast not particularly accurate
-}
integrate :: forall a. (Ord a, Fractional a) =>
    a ->              {- ^ Increment -}
    (a -> a -> a) ->  {- ^ Measure, realized as
                      a function taking the lower and upper bounds
                      of a (directed) interval to the mass between
                      -}
    (a -> a) ->       {- ^ Function to integrate -}
    a ->              {- ^ Lower bound of integration -}
    a ->              {- ^ Upper bound of integration -}
    a
integrate = \ d m f lb ub ->
    let contrib = \ x -> let x' = x + d in m x x' * (f x + f x') / 2
    in  sum . fmap contrib . takeWhile (ub >) $ iterate' (d +) lb