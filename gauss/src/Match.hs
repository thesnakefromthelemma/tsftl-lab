{-# LANGUAGE Haskell2010
  , BangPatterns
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , LambdaCase
  , PackageImports
  , ScopedTypeVariables
  , StandaloneDeriving
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
  ( Maybe (..)
  , Either (..)
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
  , unsafeThawPrimArray
  , generatePrimArray
  , replicatePrimArray
  , writePrimArray
  , readPrimArray
  )


-- ++ From strict:

import "tsftl-lab-gauss" Data.Tuple
  ( Tup2
      ( Tup2 )
  , Tup4
      ( Tup4 )
  )

import "tsftl-lab-gauss" Data.Maybe
  ( Maybe
      ( Just
      , Nothing
      )
  )

import "tsftl-lab-gauss" Data.Either
  ( Either
      ( Left
      , Right
      )
  )


-- ++ (internal):

import Data.Primitive.PrimArray.Slice
  ( MutablePrimArraySlice
      ( MutablePrimArraySlice )
  )

import qualified Data.Primitive.PrimArray.Slice as WS
  ( sortBy
  , qmMapSortedBy
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
  , ValStatusChunk
  , valStatusChunkSize
  , valStatusChunkInit
  , updateValStatusChunk
  , valStatusChunkToList
  , unValStatusChunkList
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
        _sizex :: !Int
      , _sizey' :: !Int
      , _keyArr :: !(MutablePrimArray s KeyStatus)
      , _valArr :: !(MutablePrimArray s ValStatusChunk)
      , _phase :: !Phase
      , _lagPtr :: !Int
      , _leadPtr :: !Int } ->
        Ref s

{-# INLINE pack #-}
pack :: forall s.
    Ref s -> ST s Pool
pack = \ (Ref nx ny' wk wa _ j _) -> do
    sk <- WS.unsafeFreezeToList $ MutablePrimArraySlice wk j nx
    sj <- (elemIndices Alive . unValStatusChunkList <$>) . WS.unsafeFreezeToList $ MutablePrimArraySlice wa 0 ny'
    pure $ Pool sk sj

{-# INLINE matchInit #-}
matchInit :: forall s.
    CountMat -> ST s (Ref s)
matchInit = \ (CountMat nx ny f) -> do
    wk <- unsafeThawPrimArray . generatePrimArray nx $ \ x ->
        foldl' (\ k@(KeyStatus _ m l) y -> case f x y of
            Zero -> k
            One  -> KeyStatus x (m + 1) l
            Many -> KeyStatus x m (l + 1)
          ) (KeyStatus x 0 0) [0 .. ny - 1]
    WS.sortBy compare $ MutablePrimArraySlice wk 0 nx
    let !(!qy, !ry) = quotRem ny valStatusChunkSize
        !ny' = case ry of
            0 -> qy
            _ -> qy + 1
    wa <- unsafeThawPrimArray . replicatePrimArray ny' $ valStatusChunkInit valStatusChunkSize
    case ry of
        0 -> pure ()
        _ -> writePrimArray wa qy $ valStatusChunkInit ry
    pure $ Ref nx ny' wk wa Lag 0 0

{-# INLINE matchStep #-}
matchStep :: forall s.
    (Int -> Int -> Count) -> Ref s -> ST s (Either Pool (Tup2 (Match Int Int) (Ref s)))
matchStep = \ f -> \case
    r@(Ref nx ny' wk wa Lag  j i) -> case compare nx j of
        GT -> readPrimArray wk j >>= \case
            KeyStatus x 0 0 -> pure . Right . Tup2 (Single x) $ Ref nx ny' wk wa Lag (j + 1) i
            _               -> matchStep f $ Ref nx ny' wk wa Lead j (max j i)
        _  -> Left <$> pack r
    r@(Ref nx ny' wk wa Lead j i) -> case compare nx i of
        GT -> readPrimArray wk i >>= \case
            KeyStatus _ 0 1 -> matchStep f $ Ref nx ny' wk wa Lead j (i + 1)
            KeyStatus x 1 0 ->
                WS.findIndex (\ qy a ->
                    foldr (\ (Tup2 ry v) mtqyryv -> let !y = valStatusChunkSize * qy + ry in
                        case (Alive == v) && (One == f x y) of
                            True  -> Just $ Tup4 y qy ry a
                            False -> mtqyryv
                      ) Nothing $ valStatusChunkToList a
                  ) (MutablePrimArraySlice wa 0 ny') >>= \case
                        Just (Tup4 y qy ry a) -> do
                            WS.qmMapSortedBy compare (\ k@(KeyStatus x' m' l') -> case f x' y of
                                    Zero -> k
                                    One  -> KeyStatus x' (m' - 1) l'
                                    Many -> KeyStatus x' m' (l' - 1)
                             ) $ MutablePrimArraySlice wk j nx
                            writePrimArray wa qy $ updateValStatusChunk ry Dead a 
                            pure . Right . Tup2 (Couple x y) $ Ref nx ny' wk wa Lag (j + 1) (i + 1)
                        Nothing                 -> error "Invariant failure in 'gauss:Match.matchStep' (Impossible 'Ref' value!)"
            _               -> Left <$> pack r
        _  -> Left <$> pack r

{-# INLINE match #-}
match :: CountMat -> Stalk Pool (Match Int Int)
match = \ q@(CountMat _ _ f) ->
    unfoldST (matchStep f) (matchInit q)