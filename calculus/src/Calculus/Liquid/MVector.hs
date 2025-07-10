{-# LANGUAGE Haskell2010
  , AllowAmbiguousTypes
  , KindSignatures
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  #-}

{-# OPTIONS_GHC
    -fplugin=LiquidHaskell
  #-}

{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.Liquid.MVector where

import Data.Kind
  ( Type )

import Control.Monad.ST
  ( ST
  , runST
  )

import Data.List
  ( scanl'
  , iterate'
  )

import Control.Monad
  ( (<=<) )

import qualified Data.Vector.Generic as V
  ( Vector
  , Mutable
  , fromList
  , thaw
  )

import Data.Vector.Generic.Mutable_LHAssumptions
  ( )

import qualified Data.Vector.Generic.Mutable as W
  ( unsafeWrite
  , unsafeInit
  , length
  , unsafeRead
  )

import Data.Vector.Generic.Mutable_LHAssumptions
  ( )


-- * (Discrete) calculus

{- | Passes on (the knowable initial segment of)
the discrete derivative of a given vector of 'Num' values,
assumed to be sequential with increment 1,
as well as the evaluation of the former at 0,
until we know 'Nothing'\;
computed in place, captured by the 'Control.Monad.ST.ST' monad
-}
{-# INLINE diff #-}
{-@ diff :: forall v s a. (V.Vector v a, Num a) => va : V.Mutable v s a -> ST s (Maybe (a, {va' : V.Mutable v s a | lenW va - 1 = lenW va'})) @-}
diff :: forall (v :: Type -> Type) s a.
    (V.Vector v a, Num a) =>
    V.Mutable v s a -> ST s (Maybe (a, V.Mutable v s a))
diff = \wa -> do
    let len' = W.length wa - 1
    case compare 0 len' of
        GT -> pure Nothing
        _  -> do
            a0 <- W.unsafeRead wa 0
            let wa' = W.unsafeInit wa
                {-# INLINE diffR #-}
                {-@ diffR :: {i : Nat | len' >= i} -> a -> ST s ({wa'' : V.Mutable v s a | len' = lenW wa''}) @-}
                diffR = \i -> \a -> case compare len' i of
                    GT -> do
                        let i' = i + 1
                        a' <- W.unsafeRead wa i'
                        W.unsafeWrite wa' i (a' - a)
                        diffR i' a'
                    _  -> pure wa'  
            Just . (a0,) <$> diffR 0 a0


{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given vector of 'Num' values,
assumed to be sequential with increment 1\;
the implementation caches each intermediate derivative
as a generic 'Data.Vector.Generic.Vector.Mutable' instance
threaded into the 'Control.Monad.ST.ST' monad,
its parametrizing immutable vector type represented
by the ambiguous type variable @v@, which is
__necessarily specialized by type application at the call site__.
-}
{-# INLINE taylor #-}
{-@ taylor :: forall v a. (V.Vector v a, Num a) => sa : [a] -> {sa' : [a] | len sa = len sa'} @-}
taylor :: forall (v :: Type -> Type) a.
    (V.Vector v a, Num a) =>
    [a] -> [a]
taylor = \sa ->
  let {-# INLINE unfoldrMDiff #-} -- /Inlined so that liquidhaskell can \"see through\" it/
      {-@ unfoldrMDiff :: forall s. wa : V.Mutable v s a -> ST s ({sa : [a] | lenW wa = len sa}) @-}
      unfoldrMDiff = \s -> do
          mpas <- diff s
          case mpas of
              Just (a, s') -> (a :) <$> unfoldrMDiff s'
              Nothing      -> pure []
  in  runST $ (unfoldrMDiff <=< V.thaw . V.fromList @v) sa

{- | The \(n^{\text{th}}\) row of Pascal's triangle
as a list of \(n+1\) 'Integral' values,
where \(n\) is the argument\;
for all but the \(n\) closest to zero,
an arbitrary precision 'Integral' type
should be used!
-}
{-# INLINE pascal #-}
{-@ lazy pascal @-} -- /Would it have been better to have some infinite supertype of 'List', to encode conditional finiteness?/
pascal :: forall n. Integral n => n -> [n]
pascal = \n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\b a -> b * (n - a + 1) `quot` a) 1 $ iterate' (1+) 1 -- /@[1..]@ expanded to @iterate' (1+) 1@ for liquidhaskell's benefit/

{- | Extrapolates a given finite list of 'Num' values,
assumed to be sequential from 0 with increment 1,
to a polynomial function of minimal degree
with arguments 'Integral' values;
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