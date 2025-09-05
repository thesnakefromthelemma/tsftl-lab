{-# LANGUAGE Haskell2010
  , BangPatterns
  , DerivingStrategies
  , GADTSyntax
  , KindSignatures
  , LambdaCase
  , ScopedTypeVariables
  , StandaloneDeriving
  , TupleSections
  , TypeApplications
  , NoMonoLocalBinds
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


-- + From vector / vector-algorithms:

import qualified Data.Vector.Generic.Mutable as W
  ( PrimMonad
  , PrimState
  , MVector
  , replicate
  , generate
  , unsafeWrite
  , unsafeDrop
  , unsafeRead
  , length
  )

import qualified Data.Vector.Algorithms.Merge as W
  ( sort )

import qualified Data.Vector.Generic as V
  ( unsafeFreeze
  , toList
  )

import qualified Data.Vector.Unboxed.Mutable as WU
  ( MVector )


-- ++ (internal)

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

data Switch where
    Lag :: Switch
    Lea :: Switch

data Ref :: Type -> Type where
    Ref :: forall s. {
        _length :: !Int ,
        _keyRef :: {-# UNPACK #-} !(WU.MVector s KeyStatus) ,
        _valRef :: {-# UNPACK #-} !(WU.MVector s ValStatus) ,
        _active :: !Switch ,
        _lagPtr :: !Int ,
        _leaPtr :: !Int } ->
        Ref s

{-# INLINE unsafePack #-}
unsafePack :: forall s.
    Ref s -> ST s Pool
unsafePack = \ (Ref _ wk wv _ _ i) -> do
    sk <- (V.toList <$>) . V.unsafeFreeze $ W.unsafeDrop i wk
    sj <- elemIndices Alive . V.toList <$> V.unsafeFreeze wv
    pure $ Pool sk sj

{- | 'Data.Vector.Generic.ifoldr''
    necessarily traverses the entire input!
-}
{-# INLINE findIndex #-}
findIndex :: forall (w :: Type -> Type -> Type) (m :: Type -> Type) a.
    (W.PrimMonad m, W.MVector w a) =>
    (Int -> a -> Bool) -> w (W.PrimState m) a -> m (Maybe Int)
findIndex = \ p wa ->
    let !len = W.length wa
        findR = \ i -> case compare len i of
            GT -> do
                a <- W.unsafeRead wa i
                case p i a of
                    True  -> pure $ Just i
                    False -> findR $ i + 1
            _  -> pure Nothing
    in  findR 0

{-# INLINE mapSortedOn #-}
mapSortedOn :: forall (w :: Type -> Type -> Type) (m :: Type -> Type) a b.
    (W.PrimMonad m, W.MVector w a, Ord b) =>
    (a -> b) -> (a -> a) -> w (W.PrimState m) a -> m ()
mapSortedOn = \ p f wa ->
    let !len = W.length wa
        mapSortedOnFromR = \ b0 i0 i -> case compare len i of
            GT -> do
                a <- W.unsafeRead wa i
                let !a' = f a
                    !b' = p a'
                case compare b0 b' of
                    GT -> do
                        a0' <- W.unsafeRead wa i0 -- /Assumes inversions are infrequent and short-range; cache this for better results otherwise?/
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
    in  case compare len 0 of
            GT -> do
                a0 <- W.unsafeRead wa 0
                let !a0' = f a0
                    !b0' = p a0'
                W.unsafeWrite wa 0 a0'
                mapSortedOnFromR b0' 0 1
            _  -> pure ()

{-# INLINE matchStep #-}
matchStep :: forall s.
    (Int -> Int -> Count) -> Ref s -> ST s (Either Pool (Match Int Int, Ref s))
matchStep = \ f -> \case
    r@(Ref n wk wv Lag j i) -> case compare n j of
        GT -> W.unsafeRead wk j >>= \case
            KeyStatus x 0 0 -> pure . Right . (Single x,) $ Ref n wk wv Lag (j + 1) i
            _               -> matchStep f $ Ref n wk wv Lea j (max j i)
        _  -> Left <$> unsafePack r
    r@(Ref n wk wv Lea j i) -> case compare n i of
        GT -> W.unsafeRead wk i >>= \case
            KeyStatus _ 0 1 -> matchStep f $ Ref n wk wv Lea j (i + 1)
            KeyStatus x 1 0 ->
                let matchStepV = \ y v ->
                        (Alive == v) && (One == f x y)
                in  findIndex matchStepV wv >>= \case
                        Just y  -> do
                            let matchStepH = \ k@(KeyStatus x' m' l') -> case f x' y of
                                    Zero -> k
                                    One  -> KeyStatus x' (m' - 1) l'
                                    Many -> KeyStatus x' m' (l' - 1)
                            mapSortedOn (\ k -> matchCount k + blockCount k) matchStepH $ W.unsafeDrop j wk
                            W.unsafeWrite wv y Dead
                            pure . Right . (Couple x y,) $ Ref n wk wv Lag (j + 1) (i + 1)
                        Nothing -> error "Invariant failure in 'gauss:Match.matchStep' (Impossible 'Ref' value!)"
            _               -> Left <$> unsafePack r
        _  -> Left <$> unsafePack r

{-# NOINLINE noinlineSort #-}
noinlineSort :: (W.PrimMonad m, W.MVector v e, Ord e) => v (W.PrimState m) e -> m ()
noinlineSort = W.sort

{-# INLINE match #-}
match :: CountMat -> Stalk Pool (Match Int Int)
match = \ (CountMat n f) ->
    let r = do
            let matchH' = \ k@(KeyStatus x m l) y -> case f x y of
                    Zero -> k
                    One  -> KeyStatus x (m + 1) l
                    Many -> KeyStatus x m (l + 1)
                matchH = \ x ->
                    foldl' matchH' (KeyStatus x 0 0) [0 .. n - 1]
            wk <- W.generate n matchH
            noinlineSort wk
            wv <- W.replicate n Alive
            pure @(ST _) $ Ref n wk wv Lag 0 0
    in  unfoldST (matchStep f) r