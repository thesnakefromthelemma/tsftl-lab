{-# LANGUAGE Haskell2010
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , LambdaCase
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , NoMonoLocalBinds
#-}

{-# OPTIONS_GHC
    -fexpose-all-unfoldings
    -fspecialize-aggressively
    -funbox-strict-fields
    -Wall
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
  , Stalk
      ( Yield
      , Extend
      )
  , yield
  , Match
      ( Single
      , Couple
      )
  , KeyStatus
      ( KeyStatus
      , name
      , total
      , matches
      )
  , Pool
      ( Pool
      , keys
      , valInds
      )
  -- * Matching
  , match
  ) where


-- * Imports

-- * base

import Data.Kind
  ( Type )

import Control.Monad.ST
  ( ST )

import Data.List
  ( elemIndices )


-- * vector / vector-algorithms

import qualified Data.Vector.Generic.Mutable as W
  ( PrimMonad
  , PrimState
  , MVector
  , replicate
  , generate
  , unsafeWrite
  , unsafeTail
  , unsafeRead
  , length
  )

import qualified Data.Vector.Algorithms.Merge as W
  ( sort )

import qualified Data.Vector.Generic as V
  ( freeze
  , toList
  )

import qualified Data.Vector.Unboxed.Mutable as WU
  ( MVector )


-- ** (internal)

import Data.Stalk
  ( Stalk
      ( Yield
      , Extend
      )
  , yield
  )

import Data.Stalk.UnfoldST
  ( unfoldST )

import Data.Match.KeyStatus
  ( KeyStatus
      ( KeyStatus
      , name
      , total
      , matches
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

data Ref :: Type -> Type where
    Ref :: forall s. {
        _keyRef :: !(WU.MVector s KeyStatus) ,
        _valRef :: !(WU.MVector s ValStatus) } ->
        Ref s

{-# INLINE freeze #-}
freeze :: forall s.
    Ref s -> ST s Pool
freeze = \ (Ref wk wv) -> do
    sk <- V.toList <$> V.freeze wk
    sj <- elemIndices Alive . V.toList <$> V.freeze wv
    pure $ Pool sk sj

{- | 'Data.Vector.Generic.ifoldr''
    always traverses the entire input!
-}
{-# INLINE findIndex #-}
findIndex :: forall (w :: Type -> Type -> Type) (m :: Type -> Type) a.
    (W.PrimMonad m, W.MVector w a) =>
    (Int -> a -> Bool) -> w (W.PrimState m) a -> m (Maybe Int)
findIndex = \ p wa ->
    let len = W.length wa
        findR = \ i -> case compare len i of
            GT -> do
                a <- W.unsafeRead wa i
                case p i a of
                    True  -> pure $ Just i
                    False -> findR $ i + 1
            _  -> pure Nothing
    in  findR 0

{-# INLINE mapSortedOnFrom #-}
mapSortedOnFrom :: forall (w :: Type -> Type -> Type) (m :: Type -> Type) a b.
    (W.PrimMonad m, W.MVector w a, Ord b) =>
    b -> (a -> b) -> (a -> a) -> w (W.PrimState m) a -> m ()
mapSortedOnFrom = \ lb p f wa ->
    let len = W.length wa
        mapSortedOnFromR = \ b0 i0 i -> case compare len i of
            GT -> do
                a <- W.unsafeRead wa i
                let a' = f a
                    b' = p $ f a
                case compare b0 b' of
                    GT -> do
                        a0' <- W.unsafeRead wa i0 -- /Assumes inversions are infrequent and short-range; cache this for better results otherwise/
                        W.unsafeWrite wa i a0'
                        W.unsafeWrite wa i0 a'
                        mapSortedOnFromR b0 (i0 + 1) $ i + 1
                    EQ  -> do
                        W.unsafeWrite wa i a'
                        mapSortedOnFromR b0 i0 $ i + 1
                    LT  -> do
                        W.unsafeWrite wa i a'
                        mapSortedOnFromR b' i $ i + 1
            _  -> pure ()
    in  mapSortedOnFromR lb 0 0

{-# INLINE matchStep #-}
matchStep :: forall s.
    (Int -> Int -> Count) -> Ref s -> ST s (Either Pool (Match Int Int, Ref s))
matchStep = \ f r@(Ref wk wv) ->
    case W.length wk of
        0 -> Left <$> freeze r
        _ -> W.unsafeRead wk 0 >>= \case
            KeyStatus x 1 1 ->
                let matchStepV = \ y v ->
                        (Alive == v) && (One == f x y)
                in  findIndex matchStepV wv >>= \case
                        Just y  -> do
                            let wk' = W.unsafeTail wk
                                matchStepH = \ k@(KeyStatus x' t' m') -> case f x' y of
                                    Zero -> k
                                    One  -> KeyStatus x' (t' - 1) (m' - 1)
                                    Many -> KeyStatus x' (t' - 1) m'
                            mapSortedOnFrom 0 total matchStepH wk'
                            W.unsafeWrite wv y Dead
                            pure $ Right (Couple x y, Ref wk' wv)
                        Nothing -> error "Invariant failure in 'gauss:Match.matchStep' (impossible Ref)!"
            _               -> Left <$> freeze r

{-# INLINE match #-}
match :: CountMat -> Stalk Pool (Match Int Int)
match = \ (CountMat n f) ->
    let r = do
            let matchH' = \ x k@(KeyStatus _x t m) y -> case f x y of
                    Zero -> k
                    One  -> KeyStatus x (t + 1) (m + 1)
                    Many -> KeyStatus x (t + 1) m
                matchH = \ x ->
                    foldl' (matchH' x) (KeyStatus x 0 0) [0 .. n - 1]
            wk <- W.generate n matchH
            W.sort wk
            wv <- W.replicate n Alive
            pure @(ST _) $ Ref wk wv
    in  unfoldST (matchStep f) r