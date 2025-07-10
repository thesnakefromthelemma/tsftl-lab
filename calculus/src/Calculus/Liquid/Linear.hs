{-# LANGUAGE Haskell2010
  , BangPatterns
  , GADTs
  , KindSignatures
  , LinearTypes
  , ScopedTypeVariables
  #-}

{-# OPTIONS_GHC
    -fplugin=LiquidHaskell
  #-}

{- | Least-degree polynomial interpolation
of a function defined on an evenly-spaced set of inputs
via its discrete taylor series
-}
module Calculus.Liquid.Linear where

import qualified Prelude.Linear as L

import Data.Kind
  ( Type )

import Unsafe.Linear as UL
  ( toLinear2 )

import Data.List
  ( scanl'
  , iterate'
  )

import qualified Data.Array.Mutable.Linear as W
  ( Array
  , fromList
  , unsafeSet
  , size
  , unsafeGet
  )

import Data.Array.Mutable.Linear_LHAssumptions
  ( )


-- * Strict helper type (because I'm Pair-anoid)

{- | A size paired with a contiguous chunk of
linearly mutable memory at least as large\;
this is a hack to account for that 'Data.Array.Mutable.Linear'
does not seem to export an /O(1)/ initial slice method
-}
{-@ data Init a = Init {iSize :: Nat, _ :: {wa : W.Array a | lenW wa >= iSize}} @-}
data Init :: Type -> Type where
    Init :: forall a. !Int -> !(W.Array a) %1-> Init a


-- * (Discrete) calculus

{- | (The knowable initial segment of)
the discrete derivative of a given array of 'Num' values,
assumed to be sequential with increment 1\;
performed in place with reads and writes
to the backing linear mutable array forced
-}
{-# INLINE diff #-}
{-@ diff :: forall a. (L.Movable a, Num a) => ia : Init a -> Maybe (a, {ia' : Init a | iSize ia - 1 = iSize ia'}) @-}
diff :: forall a. (L.Movable a, Num a) =>
    Init a %1-> Maybe (a, Init a)
diff = \(Init iLen wa) ->
    let iLen' = iLen - 1
        {-# INLINE diffR #-}
        {-@ diffR :: {wa' : W.Array a | lenW wa = lenW wa'} -> {i : Nat | iLen' >= i} -> a -> {wa''' : W.Array a | lenW wa = lenW wa'''} / [iLen' - i] @-}
        diffR :: W.Array a %1-> Int -> a -> W.Array a
        diffR = \wa' -> \i -> \a ->
            case compare iLen' i of
                GT ->
                    let i' = i + 1
                        %1 !(L.Ur !a', !wa'') = W.unsafeGet i' wa'
                        %1 !wa''' = W.unsafeSet i (a' - a) wa''
                    in  diffR wa''' i' a'
                _  -> wa'
    in  case iLen' of
            -1 -> L.lseq wa Nothing
            _  ->
                let %1 !(L.Ur !a, !wa') = W.unsafeGet 0 wa
                    %1 !wa'' = diffR wa' 0 a
                in  Just (a, Init iLen' wa'')

{- | (The knowable initial segment of)
the discrete Taylor coefficients of a given array of 'Num' values,
assumed to be sequential with increment 1
-}
{-# INLINE taylor #-}
{-@ taylor :: forall a. (L.Movable a, Num a) => sa : [a] -> {sa' : [a] | len sa = len sa'} @-}
taylor :: forall a. (L.Movable a, Num a) =>
    [a] -> [a]
taylor = \sa -> W.fromList sa L.$ \wa ->
    let %1 !(L.Ur !len, !wa') = W.size wa
        {-# INLINE cons #-}
        {-@ assume cons :: forall a. a -> sa : [a] -> {sa' : [a] | len sa + 1 = len sa'} @-}
        cons :: forall a. a %1-> [a] %1-> [a]
        cons = UL.toLinear2 (:)       
        {-# INLINE unfoldrDiff #-}
        {-@ unfoldrDiff :: ia : Init a -> {sa' : [a] | iSize ia = len sa'} / [iSize ia] @-}
        unfoldrDiff :: Init a %1-> [a] -- /LinearTypes couldn't infer this on its own?/
        unfoldrDiff = \ia -> case diff ia of
            Just (a, ia') -> cons a L.$ unfoldrDiff ia' -- /Can't be __that__ unsafe as this is how 'Prelude.Linear.unfoldr' is defined internally!/
            Nothing       -> []
    in unfoldrDiff L.$ Init len wa'

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
    scanl' (\b a -> b * (n - a + 1) `quot` a) 1 $ iterate' (1+) 1

{- | Extrapolates a given finite list of 'Num' values,
assumed to be sequential from 0 with increment 1,
to a polynomial function of minimal degree
with arguments 'Integral' values
-}
{-# INLINE extrapolate #-}
extrapolate :: forall a n. (L.Movable a, Num a, Integral n)
    => [a] -> n -> a
extrapolate = \sa ->
    sum . zipWith (*) (taylor sa) . fmap fromIntegral . pascal