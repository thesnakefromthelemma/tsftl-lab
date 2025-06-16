{-# LANGUAGE Haskell2010
  , ScopedTypeVariables
  #-}

module Calculus.MVector where

import Control.Monad.ST
  ( ST
  , runST
  )

import Data.Function
  ( fix )

import Data.List
  ( scanl' )

import Control.Monad.Trans.State
  ( StateT
  , evalStateT
  , put
  , get
  )

import qualified Data.Vector.Generic as V
  ( replicateM
  , fromList
  , length
  , toList
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


-- * (Discrete) Calculus

-- | (The knowable initial segment of)
-- the discrete derivative of a given list of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE diff #-}
diff :: forall s a. Num a => StateT (WS.MVector s a) (ST s) a
diff = do
    wa <- get
    a0 <- W.read wa 0
    let wa' = W.tail wa
        len' = W.length wa'
    put =<< ( fix $ \diffR -> \i -> \a -> case compare i len' of
        LT -> do
            a' <- W.read wa' i
            W.write wa' i (a' - a)
            diffR (i + 1) a'
        _  -> pure wa'
      ) 0 a0
    pure a0


-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given list of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
taylor :: forall a. Num a => VS.Vector a -> VS.Vector a
taylor = \va -> runST $
    evalStateT (V.replicateM (V.length va) diff) =<< V.thaw va

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
extrapolate :: forall a n. (Num a, Integral n) => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (V.toList . taylor $ V.fromList sa) . fmap fromIntegral . pascal