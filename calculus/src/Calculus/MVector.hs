{-# LANGUAGE Haskell2010
  , CPP
  , KindSignatures
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  #-}

#if EAGER || BOXED
{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.MVector where

import Data.Kind
  ( Type )

import Control.Monad.ST
  ( ST
  , runST
  )

import Data.List
  ( scanl' )

import Data.Function
  ( fix )

import Control.Monad
  ( (<=<) )

import qualified Data.Vector.Generic as V
  ( Vector
  , Mutable
  , fromList
  , thaw
  )

import qualified Data.Vector.Generic.Mutable as W
  ( unsafeWrite
  , unsafeInit
  , length
  , unsafeRead
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


-- * Helper

{- | The monadic generalization of 'Data.List.unfoldr'\;
seems like it should be standard?
-}
{-# INLINE unfoldrM #-} -- /Apparently this didn't happen automatically?/
unfoldrM :: forall (m :: Type -> Type) s a. Monad m =>
    (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldrM = \g -> \s -> do
    mpas <- g s
    case mpas of
        Just (a, s') -> (a :) <$> unfoldrM g s'
        Nothing      -> pure []


-- * (Discrete) calculus

{- | Passes on (the knowable initial segment of)
the discrete derivative of a given vector of 'Num' values,
assumed to be sequential with increment 1,
as well as the evaluation of the former at 0,
until we know 'Nothing'\;
formed in place, captured by the 'Control.Monad.ST.ST' monad
-}
{-# INLINE diff #-}
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
            Just . (a0,) <$> ( fix $ \diffR -> \i -> \a -> case compare i len' of
                LT -> do
                    let i' = i + 1
                    a' <- W.unsafeRead wa i'
                    W.unsafeWrite wa' i (a' - a)
                    diffR i' a'
                _  -> pure wa'              
              ) 0 a0


{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given vector of 'Num' values,
assumed to be sequential with increment 1
#if EAGER && BOXED
The implementation caches each intermediate derivative
as an unboxed mutable array (i.e., 'Data.Vector.Unboxed.Mutable.MVector')
threaded into the 'Control.Monad.ST.ST' monad.
#elif EAGER
The implementation caches each intermediate derivative
as a strict boxed mutable array (i.e., 'Data.Vector.Strict.Mutable.MVector')
threaded into the 'Control.Monad.ST.ST' monad.
#else
The implementation caches each intermediate derivative
as a lazy mutable array (i.e., 'Data.Vector.Mutable.MVector')
threaded into the 'Control.Monad.ST.ST' monad.
#endif
-}
{-# INLINE taylor #-}
#if EAGER && BOXED
taylor :: forall a. Num a => [a] -> [a]
taylor = \sa ->
    runST $ (unfoldrM diff <=< V.thaw . V.fromList @VE.Vector) sa
#elif EAGER
taylor :: forall a. (VU.Unbox a, Num a) => [a] -> [a]
taylor = \sa ->
    runST $ (unfoldrM diff <=< V.thaw . V.fromList @VU.Vector) sa
#elif BOXED
taylor :: forall a. Num a => [a] -> [a]
taylor = \sa ->
    runST $ (unfoldrM diff <=< V.thaw . V.fromList @VL.Vector) sa
#endif

{- | The \(n^{\text{th}}\) row of Pascal's triangle
as a list of \(n+1\) 'Integral' values,
where \(n\) is the argument
-}
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\b a -> b * (n - a + 1) `quot` a) 1 [1..]

{- | Extrapolates a given finite list of 'Num' values,
assumed to be sequential from 0 with increment 1,
to a polynomial function of minimal degree
with arguments 'Integral' values
-}
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


-- Footer --
#else
module Calculus.MVector ( ) where
#endif