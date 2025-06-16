{-# LANGUAGE
      GADTs
    , KindSignatures
    , LambdaCase
    , ScopedTypeVariables
    , StandaloneDeriving
    #-}

module Gauss
    ( test
    ) where


-- IMPORTS --

import Misc
    ( (<*$) )

import Data.Kind
    ( Type )

import Data.List
    ( foldl'
    , scanl'
    )

import qualified Data.Vector.Unboxed as VU
    ( Vector )

import qualified Data.Vector.Strict as VS
    ( Vector
    , fromList
    , indexed
    , (!)
    , toList
    )

import Control.Monad.State.Strict
    ( StateT
        ( StateT )
    , runStateT
    , State
    , state
    , runState
    )


-- MISC --

data SPair :: Type -> Type -> Type where
    MkSPair :: forall a b. {sFst :: !a, sSnd :: !b } -> SPair a b

deriving instance Eq SPair

liftMaybeSPair :: forall a b. SPair a Maybe b -> Maybe (SPair a b)
liftMaybeSPair = \case
    MkSPair a (Just b) -> Just $ MkSPair a b
    MkSPair _ Nothing  -> Nothing

foldrJust :: forall (t :: Type -> Type) a b. Foldable t => (a -> b -> b) -> b -> t (Maybe a) -> b
foldrJust = \g ->
    foldr (\case Just a -> g a; Nothing -> id)

toListJust :: forall (t :: Type -> Type) a. Foldable t => t (Maybe a) -> [a]
toListJust = foldrJust (:) []

foldrWhileJust :: forall (t :: Type -> Type) a b. Foldable t => (a -> b -> b) -> b -> t (Maybe a) -> b
foldrWhileJust = \g -> \b ->
    foldr (\case Just a -> g a; Nothing -> const b) b

toListWhileJust :: forall (t :: Type -> Type) a. Foldable t => t (Maybe a) -> [a]
toListWhileJust = foldrWhileJust (:) []

type VecU = VU.Vector

type VecS = VS.Vector

{-
indexToList :: forall a. VecS a -> VecS (SPair Int a)
indexToList = zipWith MkSPair [0..] . VS.toList

index :: forall a. VecS a -> VecS (SPair Int a)
index = VS.fromList . indexToList
-}


-- MATCHING ALGORITHM --

data Count :: Type where
    Zero, One, Many :: Count

deriving instance Eq Count

traceWhileJust :: forall s a. [StateT s Maybe a] -> State s [a]
traceWhileJust = \case
    []      -> pure []
    x : sx' -> state $ \s ->
        let histWhileJust :: [(a, s)]
            histWhileJust = toListWhileJust . fmap (runStateT <*$ s) $ scanl' (>>) x sx'
        in  (fmap fst histWhileJust, foldl' (flip const) s $ fmap snd histWhileJust)

matchI :: forall a b. (a -> b -> Count) -> StateT (VecS a, VecS b) Maybe (VecS (SPair a b))
matchI = \p -> StateT $ \(sa, sb) ->
    let isBasal :: a -> Maybe Int
        isBasal = \a ->
            case VS.toList . VS.filter ((Zero /=) . snd) . index $ fmap (p a) sb of
                (n, One) : [] -> Just n
                _             -> Nothing
        sbasePInd :: VecS (SPair Int Int)
        sbasePInd = VS.fromList . toListJust . fmap liftMaybeSPair . index $ fmap isBasal sa
    in  case (0 ==) $ length sbasePInd of
            True  -> Nothing
            False -> Just (fmap (\(MkSPair i j) -> ) sbasePInd , (_ , _))

match :: forall a b. (a -> b -> Count) -> State (VecS a, VecS b) [VecS (SPair a b)]
match = \p ->
    let matchIP :: StateT (VecS a, VecS b) Maybe (VecS (SPair a b))
        matchIP = undefined
    in  traceWhileJust $ repeat matchIP


-- DEDEPENDENT TYPES --

type Sym = VecU Int

sSym :: Int -> Int -> VecS Sym
sSym = \d -> \n ->
    undefined

type Slant = VecU Int

sSlant :: Int -> Int -> VecS Slant
sSlant = \d' -> \n ->
    undefined

type ITerm = (Sym, Slant)

sITerm :: Int -> Int -> Int -> VecS ITerm
sITerm = \d -> \d' -> \n ->
    undefined

type OTerm = VecU Int

sOTerm :: Int -> Int -> Int -> VecS OTerm
sOTerm = \d -> \d' -> \n ->
    undefined 

pTerm :: ITerm -> OTerm -> Count
pTerm = \si -> \so ->
    undefined


-- MAIN --

test :: Int -> Int -> Int -> ([VecS (SPair ITerm OTerm)], (VecS ITerm, VecS OTerm))
test = \d -> \d' -> \n ->
    runState (match pTerm) (sITerm d d' n, sOTerm d d' n)
