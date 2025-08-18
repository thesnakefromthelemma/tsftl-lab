{-# LANGUAGE Haskell2010
  , BangPatterns -- /The bangs on the 'get's are responsible a 2.5-fold performance improvement\; those on the wa*s are mostly redundant./
  , CPP
  , GADTs
  , KindSignatures
  , LinearTypes
  , ScopedTypeVariables
#-}

{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.Linear where

import qualified Prelude.Linear as L

import Data.Kind
  ( Type )

import Data.List
  ( scanl' )

import qualified Data.Array.Mutable.Linear as W -- /I'd love to make this 'Data.Vector.Mutable.Linear', but that worsens perfromance at least 15-fold!/
  ( Array
  , fromList
  , unsafeSet -- /Alongside 'unsafeGet', responsible for 2-fold performance increase/
  , size
  , unsafeGet -- /Alongside 'unsafeSet', responsible for 2-fold performance increase/
  )


-- * Strict helper type (because I'm Pair-anoid)

{- | A size paired with a contiguous chunk of
linearly mutable memory at least as large\;
this is a hack to account for that 'Data.Array.Mutable.Linear'
does not seem to export an /O(1)/ initial slice method
-}
data Init :: Type -> Type where
    Init :: forall a. !Int -> !(W.Array a) %1-> Init a


-- * (Discrete) calculus

{- | (The knowable initial segment of)
the discrete derivative of a given array of 'Num' values,
assumed to be sequential with increment 1
#if EAGER
Performed in place with reads and writes
to the backing linear mutable array forced
#else
Performed in place with reads and writes
to the backing linear mutable array unforced
#endif
-}
{-# INLINE diff #-}
diff :: forall a. (L.Movable a, Num a) =>
    Init a %1-> Maybe (a, Init a)
#if EAGER
diff = \ (Init len wa) ->
    let !len' = len - 1
        diffR :: W.Array a %1-> Int -> a -> W.Array a -- /Operating on 'Int's linearly worsens performance 6-fold!/
        diffR = \ wa' i a ->
            case compare len' i of
                GT ->
                    let !i' = i + 1
                        %1 !(L.Ur !a', !wa'') = W.unsafeGet i' wa'
                        %1 !wa''' = W.unsafeSet i (a' - a) wa''
                    in  diffR wa''' i' a'
                _  -> wa'
    in  case len' of
            -1 -> L.lseq wa Nothing
            _  ->
                let %1 !(L.Ur !a, !wa') = W.unsafeGet 0 wa
                    %1 !wa'' = diffR wa' 0 a
                in  Just (a, Init len' wa'')
#else
diff = \ (Init len wa) ->
    let len' = len - 1
        diffR :: W.Array a %1-> Int -> a -> W.Array a -- /Operating on 'Int's linearly worsens performance 6-fold!/
        diffR = \ wa' i a ->
            case compare len' i of
                GT ->
                    let i' = i + 1
                        %1 !(L.Ur a', wa'') = W.unsafeGet i' wa'
                        %1 !wa''' = W.unsafeSet i (a' - a) wa''
                    in  diffR wa''' i' a'
                _  -> wa'
    in  case len' of
            -1 -> L.lseq wa Nothing
            _  ->
                let %1 !(L.Ur a, wa') = W.unsafeGet 0 wa
                    %1 !wa'' = diffR wa' 0 a
                in  Just (a, Init len' wa'')
#endif

{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given array of 'Num' values,
assumed to be sequential with increment 1
-}
{-# INLINE taylor #-}
taylor :: forall a. (L.Movable a, Num a) =>
    [a] -> [a]
taylor = \ sa -> W.fromList sa L.$ \ wa ->
    let %1 !(L.Ur !len, !wa') = W.size wa
    in L.unfoldr diff L.$ Init len wa'

{- | The \(n^{\text{th}}\) row of Pascal's triangle
as a list of \(n+1\) 'Integral' values,
where \(n\) is the argument\;
for all but the \(n\) closest to zero,
an arbitrary precision 'Integral' type
should be used!
-}
{-# INLINE pascal #-}
pascal :: forall n. Integral n => n -> [n]
pascal = \ n -> (if n >= 0 then take $ fromIntegral n + 1 else id) $
    scanl' (\ b a -> b * (n - a + 1) `quot` a) 1 [1..]

{- | Extrapolates a given finite list of 'Num' values,
assumed to be sequential from 0 with increment 1,
to a polynomial function of minimal degree
with arguments 'Integral' values
-}
{-# INLINE extrapolate #-}
extrapolate :: forall a n. (L.Movable a, Num a, Integral n)
    => [a] -> n -> a
extrapolate = \ sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal