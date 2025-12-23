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

{-# OPTIONS_GHC -Wall #-}

module Match
  ( module Data.Match.Count
  , module Data.Match.KeyStatus
    -- * API Types
  , Match
      ( Single
      , Couple
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

import Prelude hiding
  ( Maybe
      ( Just
      , Nothing
      )
  )

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


-- ++ (internal):

import Data.Maybe.Strict
  ( Maybe
      ( Just
      , Nothing
      )
  )

import Data.Primitive.PrimArray.Slice
  ( MutablePrimArraySlice
      ( MutablePrimArraySlice )
  )

import qualified Data.Primitive.PrimArray.Slice as WS
  ( sort
  , mapSortedBy
  , findIndex
  , unsafeFreezeToList
  )

import Data.Stalk
  ( Stalk )

import Data.Stalk.UnfoldST
  ( unfoldST )

import Data.Match.Count
  ( Count
      ( Zero
      , One
      , Many
      )
  , CountMat
      ( CountMat
      , sizeIn
      , fun
      )
  )

import Data.Match.KeyStatus
  ( KeyStatus
      ( KeyStatus
      , index
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


-- * API Types

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

data Pool where
    Pool ::
      { keys :: ![KeyStatus]
      , valInds :: ![Int] } ->
        Pool

deriving stock instance Show Pool


-- * Matching in \(O(mn)\) time and \(O(m+n)\) space

data Phase where
    Lag :: Phase
    Lead :: Phase

data Ref :: Type -> Type where
    Ref :: forall s. {
        _size :: !Int
      , _keyArr :: {-# UNPACK #-} !(MutablePrimArray s KeyStatus)
      , _valArr :: {-# UNPACK #-} !(MutablePrimArray s ValStatus)
      , _phase :: !Phase
      , _lagPtr :: !Int
      , _leadPtr :: !Int } ->
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
    r@(Ref n wk wv Lag  j i) -> case compare n j of
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
                            WS.mapSortedBy compare matchStepH $ MutablePrimArraySlice wk j n
                            writePrimArray wv y Dead
                            pure . Right . (Couple x y,) $ Ref n wk wv Lag (j + 1) (i + 1)
                        Nothing -> error "Invariant failure in 'gauss:Match.matchStep' (Impossible 'Ref' value!)"
            _               -> Left <$> pack r
        _  -> Left <$> pack r

{-# INLINE match #-}
match :: CountMat -> Stalk Pool (Match Int Int)
match = \ (CountMat nx ny f) ->
    let matchA = do
            let matchHH = \ k@(KeyStatus x m l) y -> case f x y of
                    Zero -> k
                    One  -> KeyStatus x (m + 1) l
                    Many -> KeyStatus x m (l + 1)
                matchH = \ x ->
                    foldl' matchHH (KeyStatus x 0 0) [0 .. ny - 1]
            wk <- unsafeThawPrimArray $ generatePrimArray nx matchH
            WS.sort $ MutablePrimArraySlice wk 0 nx
            wv <- unsafeThawPrimArray $ replicatePrimArray ny Alive
            pure @(ST _) $ Ref nx wk wv Lag 0 0
    in  unfoldST (matchStep f) matchA