{-# LANGUAGE Haskell2010
  , KindSignatures
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
  #-}

module Calculus.MVector where

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
  ( fromList
  , thaw
  )

import qualified Data.Vector.Generic.Mutable as W
  ( write
  , tail
  , length
  , read
  )

import qualified Data.Vector.Strict as VS
  ( Vector )

import qualified Data.Vector.Strict.Mutable as WS
  ( MVector )


-- * Helper

{-# INLINE unfoldrM #-} -- /Apparently this didn't happen automatically?/
unfoldrM :: forall (m :: Type -> Type) s a. Monad m => (s -> m (Maybe (a, s))) -> s -> m [a]
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
diff :: forall s a. Num a => WS.MVector s a -> ST s (Maybe (a, WS.MVector s a))
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
taylor :: forall a. Num a => [a] -> [a]
taylor = \sa ->
    runST $ (unfoldrM diff <=< V.thaw . V.fromList @VS.Vector ) sa

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