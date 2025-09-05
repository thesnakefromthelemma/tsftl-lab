{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , LambdaCase
  , ScopedTypeVariables
  , StandaloneDeriving
  , TupleSections
  , TypeApplications
#-}

{-# OPTIONS_GHC
    -Wall
    -O2
    -fexpose-all-unfoldings
    -fspecialize-aggressively
#-}

module Match
  ( -- * API types
    Count
      ( Zero
      , One
      , Many
      )
  , CountMat
      ( CountMat
      , size
      , fun
      )
  , Match
      ( Single
      , Couple
      )
  , KeyStatus
      ( KeyStatus
      , name
      , matchCount
      , blockCount
      )
  , Pool
      ( Pool
      , keys
      , valInds
      )
  -- * Matching
  , match
  ) where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )

import Control.Monad.ST
  ( ST )

import Data.List
  ( elemIndices )


-- ++ From primitive:

import Data.Primitive.PrimArray
  ( MutablePrimArray
  , writePrimArray
  , readPrimArray
  , unsafeThawPrimArray
  , generatePrimArray
  , replicatePrimArray
  )


-- ++ (internal)

import Data.Primitive.MutablePrimArraySlice
  ( MutablePrimArraySlice
      ( MutablePrimArraySlice )
  )

import qualified Data.Primitive.MutablePrimArraySlice as WS
  ( sort
  , mapSortedOn
  , findIndex
  , unsafeFreezeToList
  )

import Data.Stalk
  ( Stalk )

import Data.Stalk.UnfoldST
  ( unfoldST )

import Data.Match.KeyStatus
  ( KeyStatus
      ( KeyStatus
      , name
      , matchCount
      , blockCount
      )
  )

import Data.Match.ValStatus
  ( ValStatus
      ( Dead
      , Alive
      )
  )


-- * API types

data Match :: Type -> Type -> Type where
    Single :: forall a b.
        !a -> -- ^ key
        Match a b
    Couple :: forall a b.
        !a -> -- ^ key
        !b -> -- ^ val
        Match a b

deriving stock instance forall a b. (Eq a, Eq b) => Eq (Match a b)
deriving stock instance forall a b. (Show a, Show b) => Show (Match a b)

data Count where
    Zero, One, Many :: Count

deriving stock instance Eq Count
deriving stock instance Ord Count
deriving stock instance Show Count

data CountMat where
    CountMat :: {
        size :: !Int ,
        fun :: Int -> Int -> Count } ->
        CountMat

data Pool where
    Pool :: {
        keys :: ![KeyStatus] ,
        valInds :: ![Int] } ->
        Pool

deriving stock instance Show Pool


-- * Matching in \(O(n^2)\) time and \(O(n)\) space

data Phase where
    Lag :: Phase
    Lead :: Phase

data Ref :: Type -> Type where
    Ref :: forall s. {
        _size :: !Int ,
        _keyArr :: {-# UNPACK #-} !(MutablePrimArray s KeyStatus) ,
        _valArr :: {-# UNPACK #-} !(MutablePrimArray s ValStatus) ,
        _phase :: !Phase ,
        _lagPtr :: !Int ,
        _leadPtr :: !Int } ->
        Ref s

{-# INLINE pack #-}
pack :: forall s.
    Ref s -> ST s Pool
pack = \ (Ref n wk wv _ j _) -> do
    sk <- WS.unsafeFreezeToList $ MutablePrimArraySlice wk j n
    sj <- (elemIndices Alive <$>) . WS.unsafeFreezeToList $ MutablePrimArraySlice wv 0 n
    pure $ Pool sk sj

{-# INLINE matchStep #-}
matchStep :: forall s.
    (Int -> Int -> Count) -> Ref s -> ST s (Either Pool (Match Int Int, Ref s))
matchStep = \ f -> \case
    r@(Ref n wk wv Lag j i) -> case compare n j of
        GT -> readPrimArray wk j >>= \case
            KeyStatus x 0 0 -> pure . Right . (Single x,) $ Ref n wk wv Lag (j + 1) i
            _               -> matchStep f $ Ref n wk wv Lead j (max j i)
        _  -> Left <$> pack r
    r@(Ref n wk wv Lead j i) -> case compare n i of
        GT -> readPrimArray wk i >>= \case
            KeyStatus _ 0 1 -> matchStep f $ Ref n wk wv Lead j (i + 1)
            KeyStatus x 1 0 ->
                let matchStepV = \ y v ->
                        (Alive == v) && (One == f x y)
                in  WS.findIndex matchStepV (MutablePrimArraySlice wv 0 n) >>= \case
                        Just y  -> do
                            let matchStepH = \ k@(KeyStatus x' m' l') -> case f x' y of
                                    Zero -> k
                                    One  -> KeyStatus x' (m' - 1) l'
                                    Many -> KeyStatus x' m' (l' - 1)
                            WS.mapSortedOn (\ k -> matchCount k + blockCount k) matchStepH $ MutablePrimArraySlice wk j n
                            writePrimArray wv y Dead
                            pure . Right . (Couple x y,) $ Ref n wk wv Lag (j + 1) (i + 1)
                        Nothing -> error "Invariant failure in 'gauss:Match.matchStep' (Impossible 'Ref' value!)"
            _               -> Left <$> pack r
        _  -> Left <$> pack r

{-# INLINE match #-}
match :: CountMat -> Stalk Pool (Match Int Int)
match = \ (CountMat n f) ->
    let matchA = do
            let matchH' = \ k@(KeyStatus x m l) y -> case f x y of
                    Zero -> k
                    One  -> KeyStatus x (m + 1) l
                    Many -> KeyStatus x m (l + 1)
                matchH = \ x ->
                    foldl' matchH' (KeyStatus x 0 0) [0 .. n - 1]
            wk <- unsafeThawPrimArray $ generatePrimArray n matchH
            WS.sort $ MutablePrimArraySlice wk 0 n
            wv <- unsafeThawPrimArray $ replicatePrimArray n Alive
            pure @(ST _) $ Ref n wk wv Lag 0 0
    in  unfoldST (matchStep f) matchA