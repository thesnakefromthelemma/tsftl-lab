{-# LANGUAGE Haskell2010
  , CPP
  , KindSignatures
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  #-}

module MVector where

import Data.Kind
  ( Type )

import Control.Monad.ST
  ( ST
  , runST
  )

import Data.Function
  ( fix )

import Data.List
  ( scanl' )

import Control.Monad
  ( (<=<) )

import qualified Data.Vector.Generic as V
  ( Vector
  , Mutable
  , fromList
  , thaw
  )

import qualified Data.Vector.Generic.Mutable as W
  ( write
  , tail
  , length
  , read
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

{-# INLINE unfoldrM #-} -- /Apparently this didn't happen automatically?/
unfoldrM :: forall (m :: Type -> Type) s a. Monad m =>
    (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldrM = \g -> fix $ \unfoldrMR -> \s -> do
    mpas <- g s
    case mpas of
        Just (a, s') -> (a :) <$> unfoldrMR s'
        Nothing      -> pure []


-- * (Discrete) Calculus

-- | Passes on (the knowable initial segment of)
-- the discrete derivative of a given vector of 'Num' values,
-- assumed to be sequential with increment 1,
-- as well as the evaluation of the former at 0,
-- until we know 'Nothing'
{-# INLINE diff #-}
diff :: forall (v :: Type -> Type) s a.
    (V.Vector v a, Num a) =>
    V.Mutable v s a -> ST s (Maybe (a, V.Mutable v s a))
diff = \wa -> do
    let len' = W.length wa - 1
    case len' of
        -1 -> pure Nothing
        _  -> do
            a0 <- W.read wa 0
            let wa' = W.tail wa
            Just . (a0,) <$> ( fix $ \diffR -> \i -> \a -> case compare i len' of
                LT -> do
                    a' <- W.read wa' i
                    W.write wa' i (a' - a)
                    diffR (i + 1) a'
                _  -> pure wa'
              ) 0 a0


-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given vector of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
#if EAGER && BOXED
taylor :: forall a. Num a => [a] -> [a]
taylor = \sa ->
    runST $ (unfoldrM diff <=< V.thaw . V.fromList @VE.Vector ) sa
#elif EAGER
taylor :: forall a. (VU.Unbox a, Num a) => [a] -> [a]
taylor = \sa ->
    runST $ (unfoldrM diff <=< V.thaw . V.fromList @VU.Vector ) sa
#elif BOXED
taylor :: forall a. Num a => [a] -> [a]
taylor = \sa ->
    runST $ (unfoldrM diff <=< V.thaw . V.fromList @VL.Vector ) sa
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