{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TypeApplications
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Slices into 'Data.Primitive.PrimArray.MutablePrimArray's
    from "Data.Primitive.PrimArray"\;
    represented as all but the same
    as a 'Data.Vector.Primitive.Mutable.MVector'
    from "Data.Vector.Primitive.Mutable",
    just without the cruft (which seriously hampers
    the legibility of dumped GHC-core)
-}
module Data.Primitive.PrimArray.Slice
  ( module Data.Maybe.Strict
    -- * Slices of 'MutablePrimArray's
  , MutablePrimArraySlice
      ( MutablePrimArraySlice
      , contents
      , start
      , end
      )
    -- 'MutablePrimArraySlice' algorithms
  , sort
  , mapSortedBy
  , findIndex
  , unsafeFreezeToList
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

import Data.Array.Byte
  ( pattern MutableByteArray )

import qualified GHC.List as List
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


-- ++ (internal):

import Data.Maybe.Strict
  ( Maybe
      ( Just
      , Nothing
      )
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

{- | Wraps the argument as an 'WP.MVector'
    and 'WP.sort's it inplace.
-}
{-# NOINLINE sort #-}
sort :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a, Ord a) =>
    MutablePrimArraySlice (PrimState m) a -> m ()
sort = \ (MutablePrimArraySlice (MutablePrimArray wa#) i0 i1) -> do
    WP.sort . WP.MVector @_ @a i0 (i1 - i0) $ MutableByteArray wa#

{- | Given a comparison function @cmp :: a -> a -> Ordering@,
    function @f :: a -> a@ with the property
    that for all @a :: a@, @cmp (f a) a = LT@ or @cmp (f a) a = EQ@ but
    there exists no @a' :: a@ such that @cmp (f a) a' = LT@ and @cmp a' a = LT@,
    and @ta :: MutablePrimArraySlice (PrimState m) a@ nondecreasing wrt @cmp@,
    maps @f@ over @ta@ while maintaining its nondecreasingness wrt @cmp@
    in a single pass using only constant auxiliary space.
-}
{-# INLINE mapSortedBy #-}
mapSortedBy :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    (a -> a -> Ordering) -> (a -> a) -> MutablePrimArraySlice (PrimState m) a -> m ()
mapSortedBy = \ cmp f (MutablePrimArraySlice wa i0 i1) ->
    case compare i0 i1 of
        LT -> do
            a0 <- readPrimArray wa i0
            let a0' = f a0
            writePrimArray wa i0 a0'
            foldr (\ i k a'S iS -> do
                a <- readPrimArray wa i
                let a' = f a
                case cmp a'S a' of
                    GT -> do
                        writePrimArray wa iS a'
                        writePrimArray wa i a'S
                        k a'S (iS + 1)
                    EQ -> do
                        writePrimArray wa i a'
                        k a'S iS
                    LT -> do
                        writePrimArray wa i a'
                        k a' i
              ) (\ _ _ -> pure ()) [i0 + 1 .. i1 - 1] a0' i0
        _  -> pure ()

{- 'Just' the first index of the second argument satisfying
    the predicate given as the first argument if one exists,
    else 'Nothing'\; for the standard 'PrimMonad' instances
    (i.e., 'IO' and 'Control.Monad.ST.ST') it is effectively
    an early-returning strict left fold.
-}
{-# INLINE findIndex #-}
findIndex :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    (Int -> a -> Bool) -> MutablePrimArraySlice (PrimState m) a -> m (Maybe Int)
findIndex = \ p (MutablePrimArraySlice wa i0 i1) ->
    foldr ( \ i xmn -> do
        a <- readPrimArray wa i
        case p i a of
            True  -> pure $ Just i
            False -> xmn
      ) (pure Nothing) [i0 .. i1 - 1]

{- | Unsafely (i.e., without copying the backing memory)
   freezes a 'MutablePrimArraySlice' to a fold/build
   fusible 'Data.List.List'
-}
{-# INLINE unsafeFreezeToList #-}
unsafeFreezeToList :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    MutablePrimArraySlice (PrimState m) a -> m [a]
unsafeFreezeToList = \ (MutablePrimArraySlice wa i0 i1) -> do
    va <- unsafeFreezePrimArray wa
    pure $ List.build $ \ g b ->
        foldr (g . indexPrimArray va) b [i0 .. i1 - 1]