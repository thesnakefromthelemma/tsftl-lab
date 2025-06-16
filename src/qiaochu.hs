{-# LANGUAGE
      DataKinds
    , GADTs
    , InstanceSigs
    , KindSignatures
    , ScopedTypeVariables
    #-}

module Qiaochu where


-- IMPORTS --

import qualified Data.Vector.Strict as VS
    ( Vector
    , singleton
    , replicate
    , zipWith
    , break
    , take
    , length
    )

import Data.Kind
    ( Type )

{-
-- POLYNOMIALS MODULO X^_ --

data Res :: Type -> Type where
    MkRes :: forall a. { precision :: Int, order :: Int, coefficients :: VS.Vector a } -> Res a

chop :: Res a -> Res a
chop = \r@(MkRes m v sc) -> case compare m (v + VS.length sc) of
    LT -> _
    _  -> r

safeRes :: forall a. (Num a, Eq a) => Int -> Int -> VS.Vector a -> Res a
safeRes = chop .<*** MkRes



instance forall a. (Eq a, Num a) => Num (Res a) where
    (+) :: Res a -> Res a -> Res a
    (+) = \(MkRes d0 sc0) -> \(MkRes d1 sc1) ->
        let len0, len1 :: Int
            (len0, len1) = (length sc0, length sc1)
            i, d :: Int
            (i, d) = (min (d0 - len0) (d1 - len1), min d0 d1)
            sc0N, sc1N :: VS.Vector a
            (sc0N, sc1N) = (VS.replicate () 0 ++ VS.take (len0 - d0 + d) sc0, _ . _)
        in  MkRes ub . snd . VS.break (/= 0) $ VS.zipWith (+) sc0N sc1N

    negate :: Res a -> Res a
    negate = fmap ((-1) *)

    (*) :: Res a -> Res a -> Res a
    (*) = \p0 -> \p1 ->
        let lb0, ub0, lb1, ub1 :: Int
            (lb0, ub0) = bounds p0
            (lb1, ub1) = bounds p1
        in  force . streamArray (lb0 + lb1, min (ub0 + lb1) (ub1 + lb0)) . fmap sum . S.wind . S.zipWith (fmap . (*)) (elemsStream p0) . S.repeat $ elemsStream p1

    fromInteger :: Integer -> Res a
    fromInteger = MkRes maxBound . VS.singleton

    abs :: Res a -> Res a
    abs = undefined -- BROKEN AS INTENDED

    signum :: Res a -> Res a
    signum = undefined -- BROKEN AS INTENDED

instance forall a. (Eq a, Fractional a, NFData a) => Fractional (Res a) where
    (/) :: Res a -> Res a -> Res a
    (/) = undefined

    fromRational :: Rational -> Res a
    fromRational = MkRes maxBound . VS.singleton
-}


-- CYCLOTOMIC NUMBERS --

