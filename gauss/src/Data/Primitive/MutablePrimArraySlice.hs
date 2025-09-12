{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TypeApplications
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Slices into 'Data.Primitive.PrimArray.MutablePrimArray's from "Data.Primitive.PrimArray"\;
    represented as all but the same as a 'Data.Vector.Primitive.Mutable.MVector' from "Data.Vector.Primitive.Mutable",
    just without the cruft (which seriously hampers the legibility of dumped GHC-core)
-}
module Data.Primitive.MutablePrimArraySlice
  ( -- * Slices of 'MutablePrimArray's
    MutablePrimArraySlice
      ( MutablePrimArraySlice
      , contents
      , start
      , end
      )
    -- 'MutablePrimArraySlice' algorithms
  , sort
  , mapSortedOn
  , findIndex
  , unsafeFreezeToList
  ) where


-- + Imports

-- ++ From base:

import Data.Kind
  ( Type )

import Data.Array.Byte
  ( pattern MutableByteArray )

import qualified GHC.Exts as List
  ( build )


-- ++ From primitive:

import Control.Monad.Primitive
  ( PrimMonad
  , PrimState
  )

import Data.Primitive.Types
  ( Prim )

import Data.Primitive.PrimArray
  ( MutablePrimArray
      ( MutablePrimArray )
  , writePrimArray
  , readPrimArray
  , unsafeFreezePrimArray
  , indexPrimArray
  )


-- + From vector / vector-algorithms:

import qualified Data.Vector.Primitive.Mutable as WP
  ( pattern MVector )

import qualified Data.Vector.Algorithms.Merge as WP
  ( sort )


-- * Slices of 'MutablePrimArray's

{- | Type of slices into @'Data.Primitive.PrimArray.MutablePrimArray' _ _@s
    from "Data.Primitive.PrimArray"
-}
data MutablePrimArraySlice :: Type -> Type -> Type where
    MutablePrimArraySlice :: forall s a. {
        contents :: {-# UNPACK #-} !(MutablePrimArray s a) , -- ^ Underlying 'Data.Primitive.PrimArray.MutablePrimArray' (itself thinly wrapping a 'GHC.Exts.MutableByteArray#')
        start :: !Int , -- ^ Index from which the slice begins\; included therein
        end :: !Int } -> -- ^ Index at which the slice ends\; excluded therefrom
        MutablePrimArraySlice s a


-- * 'MutablePrimArraySlice' algorithms

{-# INLINE sort #-}
sort :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a, Ord a) =>
    MutablePrimArraySlice (PrimState m) a -> m ()
sort = \ (MutablePrimArraySlice (MutablePrimArray wa#) i0 i1) -> do
    WP.sort . WP.MVector @_ @a i0 (i1 - i0) $ MutableByteArray wa#

{-# INLINE mapSortedOn #-}
mapSortedOn :: forall (m :: Type -> Type) a b.
    (PrimMonad m, Prim a, Ord b) =>
    (a -> b) -> (a -> a) -> MutablePrimArraySlice (PrimState m) a -> m ()
mapSortedOn = \ p f (MutablePrimArraySlice wa i0 i1) ->
    let mapSortedOnR = \ bS iS i -> case compare i1 i of
            GT -> do
                a <- readPrimArray wa i
                let a' = f a
                    b' = p a'
                case compare bS b' of
                    GT -> do
                        a0' <- readPrimArray wa iS -- /Assumes inversions are infrequent and short-range; cache this for better results otherwise?/
                        writePrimArray wa iS a'
                        writePrimArray wa i a0'
                        mapSortedOnR bS (iS + 1) $ i + 1
                    EQ  -> do
                        writePrimArray wa i a'
                        mapSortedOnR bS iS $ i + 1
                    LT  -> do
                        writePrimArray wa i a'
                        mapSortedOnR b' i $ i + 1
            _  -> pure ()
    in  case compare i1 i0 of
            GT -> do
                a0 <- readPrimArray wa i0
                let a0' = f a0
                    b0' = p a0'
                writePrimArray wa i0 a0'
                mapSortedOnR b0' i0 $ i0 + 1
            _  -> pure ()

{-# INLINE findIndex #-}
findIndex :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    (Int -> a -> Bool) -> MutablePrimArraySlice (PrimState m) a -> m (Maybe Int)
findIndex = \ p (MutablePrimArraySlice wa i0 i1) ->
    let findIndexR = \ i -> case compare i1 i of
            GT -> do
                a <- readPrimArray wa i
                case p i a of
                    True  -> pure $ Just i
                    False -> findIndexR $ i + 1
            _  -> pure Nothing
    in  findIndexR i0

{-# INLINE unsafeFreezeToList #-}
unsafeFreezeToList :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    MutablePrimArraySlice (PrimState m) a -> m [a]
unsafeFreezeToList = \ (MutablePrimArraySlice wa i0 i1) -> do
    va <- unsafeFreezePrimArray wa
    pure $ List.build $ \ g b ->
        let buildR = \ i -> case compare i1 i of
                GT -> g (indexPrimArray va i) . buildR $ i + 1
                _  -> b
        in  buildR i0