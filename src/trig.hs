{-# LANGUAGE
      FlexibleInstances
    , GADTs
    , InstanceSigs
    , KindSignatures
    , MultiParamTypeClasses
    , ScopedTypeVariables
    , StandaloneDeriving
    #-}

module Trig
    ( exp
    , cossin
    , cos
    , sin
    ) where


-- IMPORTS --

import Prelude hiding
    ( exp
    , cos
    , sin
    )

import Data.Kind
    ( Type )

import Data.List
    ( iterate'
    , scanl'
    )

import Control.Monad.State.Strict
    ( State )


-- TYPE Approx :: Type -> Type -> Type --
    
type Approx a e = State e a
    
class (Num e, Ord e) => Norm a e where
    norm :: a -> e
        
instance forall a. Num a => Norm a a where
    norm :: a -> a
    norm = abs


-- DATA Complex :: Type -> Type --

data Complex :: Type -> Type where
    MkCmplx :: forall a. { rePart :: !a, imPart :: !a } -> Complex a

deriving instance forall a. Eq a => Eq (Complex a)

instance Functor Complex where
    fmap :: forall a a'. (a -> a') -> Complex a -> Complex a'
    fmap = \f -> \(MkCmplx r s) ->
        MkCmplx (f r) (f s)

conj :: forall a. Num a => Complex a -> Complex a
conj = \(MkCmplx r s) ->
    MkCmplx r (-s)

instance forall a. (Num a, Ord a) => Norm (Complex a) a where
    norm :: Complex a -> a
    norm = \(MkCmplx r s) ->
        r ^ 2 + s ^ 2

instance forall a. Num a => Num (Complex a) where
    (+) :: Complex a -> Complex a -> Complex a
    (+) = \(MkCmplx r0 s0) -> \(MkCmplx r1 s1) ->
        MkCmplx (r0 + r1) (s0 + s1)

    negate :: Complex a -> Complex a
    negate = \(MkCmplx r s) ->
        MkCmplx (-r) (-s)

--  (-) :: Complex a -> Complex a -> Complex a

    (*) :: Complex a -> Complex a -> Complex a
    (*) = \(MkCmplx r0 s0) -> \(MkCmplx r1 s1) ->
        MkCmplx (r0 * r1 - s0 * s1) (r0 * s1 + s0 * r1)

    abs :: Complex a -> Complex a
    abs = \z ->
        MkCmplx (norm z) 0

    signum :: Complex a -> Complex a
    signum = \z ->
        z / abs z

    fromInteger :: Integer -> Complex a
    fromInteger = \n ->
        MkCmplx n 0

instance forall a. Fractional a => Fractional (Complex a) where
    recip :: Complex a -> Complex aa
    recip = \z ->
        fmap (/ norm z) $ conj z

--  (/) :: Complex a -> Complex a -> Complex a

    fromRational :: Rational -> Complex a
    fromRational = \q ->
        MkCmplx q 0


-- MATH --

exp :: forall a e. (Num a, Norm a e) => e -> a -> Approx a e
exp = undefined

cossin :: forall a e. (Num a, Norm a e) => e -> a -> Approx a e
cossin = undefined

cos :: forall a e. (Num a, Norm a e) => e -> a -> Approx a e
cos = undefined

sin :: forall a e. (Num a, Norm a e) => e -> a -> Approx a e
sin = undefined
