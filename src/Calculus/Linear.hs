{-# LANGUAGE Haskell2010
  , BangPatterns
  , GADTs
  , KindSignatures
  , LinearTypes
  , ScopedTypeVariables
  #-}

module Calculus.Linear where

import Prelude.Linear

import qualified Prelude as NL

import Data.Kind
  ( Type )

import qualified Data.List as NL.List
  ( scanl' )

import qualified Data.Array.Mutable.Linear as W
  ( Array
  , fromList
  , set
  , size
  , get
  )


-- * Strict type because I'm Pair-anoid

data MutTail :: Type -> Type where
    MutTail :: forall a. !Int -> !(W.Array a) %1-> MutTail a


-- * (Discrete) Calculus

-- | (The knowable initial segment of)
-- the discrete derivative of a given array of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE diff #-}
diff :: forall a. (Movable a, NL.Num a) =>
    MutTail a %1-> Maybe (a, MutTail a)
diff =
    let diffR :: Int -> Int -> W.Array a %1-> Int -> a -> W.Array a
        diffR = \pos -> \len -> \wa -> \i -> \a -> case compare len i of
            GT ->
                let %1 !(Ur !a', !wa') = W.get (pos + i) wa
                    %1 !wa'' = W.set (pos + i) (a' NL.- a) wa'
                in  diffR pos len wa'' (i + 1) a'
            _  -> wa
    in  \(MutTail pos wa) ->
        let %1 !(Ur !len, !wa') = W.size wa
            !posN = pos + 1
            !lenN = len - posN
        in  case lenN of
                -1 -> (\() -> Nothing) $ consume wa'
                _  ->
                    let %1 !(Ur !a, !wa'') = W.get pos wa'
                        !wa''' = diffR posN lenN wa'' 0 a
                    in  Just (a, MutTail posN wa''')

-- | (The knowable initial segment of)
-- the discrete Taylor coefficients of a given array of 'Num' values,
-- assumed to be sequential with increment 1
{-# INLINE taylor #-}
taylor :: forall a. (Movable a, NL.Num a) =>
    [a] -> [a]
taylor = \sa -> W.fromList sa $
    unfoldr diff . MutTail 0

-- | The \(n^{\text{th}}\) row of Pascal's triangle
-- as a list of \(n+1\) 'Integral' values,
-- where \(n\) is the argument
{-# INLINE pascal #-}
pascal :: forall n. NL.Integral n => n -> [n]
pascal = \n -> (if n NL.>= 0 then NL.take $ NL.fromIntegral n NL.+ 1 else id) $
    NL.List.scanl' (\b a -> b NL.* (n NL.- a NL.+ 1) `NL.quot` a) 1 [1..] -- /JFC/

-- | Extrapolates a given finite list of 'Num' values,
-- assumed to be sequential from 0 with increment 1,
-- to a polynomial function of minimal degree
-- with arguments 'Integral' values
{-# INLINE extrapolate #-}
extrapolate :: forall a n. (Movable a, NL.Num a, NL.Integral n)
    => [a] -> n -> a
extrapolate = \sa ->
    NL.sum NL.. NL.zipWith (NL.*) (taylor sa) NL.. NL.fmap NL.fromIntegral NL.. pascal -- /Lack of multiplicity polymorphic '(.)' is PITA/