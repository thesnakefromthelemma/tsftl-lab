{-# LANGUAGE Haskell2010
  , GADTSyntax
  , KindSignatures
  , PackageImports
  , ScopedTypeVariables
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
  ( module Data.Maybe
    -- * Slices of 'MutablePrimArray's
  , MutablePrimArraySlice
      ( MutablePrimArraySlice
      , contents
      , start
      , end
      )
  , sizeOfPrimArraySlice
  , writePrimArraySlice
  , readPrimArraySlice
    -- 'MutablePrimArraySlice' algorithms
  , sortBy
  , qmMapSortedBy
  , findIndex
  , unsafeFreezeToList
  ) where


-- + Imports

-- ++ From base:

import Prelude hiding
  ( Maybe (..) )

import Data.Kind
  ( Type )

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
  , writePrimArray
  , readPrimArray
  , unsafeFreezePrimArray
  , indexPrimArray
  )


-- ++ From misc:

import "tsftl-lab-gauss" Misc
  ( range
  , rangeAnti
  )


-- ++ From strict:

import "tsftl-lab-gauss" Data.Maybe
  ( Maybe (..) )


-- * Slices of 'MutablePrimArray's

-- ** 'MutablePrimArraySlice'

{- | Type of slices into @'Data.Primitive.PrimArray.MutablePrimArray' _ _@s
    from "Data.Primitive.PrimArray"
-}
data MutablePrimArraySlice :: Type -> Type -> Type where
    MutablePrimArraySlice :: forall s a. {
        contents :: !(MutablePrimArray s a) , -- ^ Underlying 'Data.Primitive.PrimArray.MutablePrimArray' (itself thinly wrapping a 'GHC.Exts.MutableByteArray#')
        start :: !Int , -- ^ Index from which the slice begins\; included therein
        end :: !Int } -> -- ^ Index at which the slice ends\; excluded therefrom
        MutablePrimArraySlice s a

{-# INLINE sizeOfPrimArraySlice #-}
sizeOfPrimArraySlice :: forall s a.
    MutablePrimArraySlice s a -> Int
sizeOfPrimArraySlice = \ (MutablePrimArraySlice _ i0 i1) ->
    i1 - i0

{-# INLINE writePrimArraySlice #-}
writePrimArraySlice :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    MutablePrimArraySlice (PrimState m) a -> Int -> a -> m ()
writePrimArraySlice = \ (MutablePrimArraySlice wsa i0 _) i a ->
    writePrimArray wsa (i0 + i) a

{-# INLINE readPrimArraySlice #-}
readPrimArraySlice :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    MutablePrimArraySlice (PrimState m) a -> Int -> m a
readPrimArraySlice = \ (MutablePrimArraySlice wsa i0 _) i ->
    readPrimArray wsa $ i0 + i


-- ** 'MutablePrimArraySlice' algorithms

{- | Artisanal heap sort\;
    not a bottleneck(?) so not particularly optimized
-}
{-# INLINE sortBy #-}
sortBy :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    (a -> a -> Ordering) -> MutablePrimArraySlice (PrimState m) a -> m ()
sortBy = \ cmp wsa -> let len = sizeOfPrimArraySlice wsa in do
    let -- | Inner loop\; not much point in writing as fold(?)
        heapifyH = \ len' k a i -> case compare len' $ 2 * i + 2 of
            GT -> do
                let iLft = 2 * i + 1
                    iRgt = 2 * i + 2
                aLft <- readPrimArraySlice wsa iLft
                aRgt <- readPrimArraySlice wsa iRgt
                case (cmp a aLft, cmp a aRgt, cmp aLft aRgt) of
                    (LT, _ , GT) -> writePrimArraySlice wsa i aLft >> heapifyH len' k a iLft
                    (_ , LT, _ ) -> writePrimArraySlice wsa i aRgt >> heapifyH len' k a iRgt
                    (_ , _ , _ ) -> writePrimArraySlice wsa i a >> k
            EQ -> do
                let iLft = 2 * i + 1
                aLft <- readPrimArraySlice wsa iLft
                case cmp a aLft of
                    LT -> writePrimArraySlice wsa i aLft >> heapifyH len' k a iLft
                    _  -> writePrimArraySlice wsa i a >> k
            LT -> writePrimArraySlice wsa i a >> k
    -- | Heapify
    foldr (\ i k -> do
        a <- readPrimArraySlice wsa i
        heapifyH len k a i
      ) (pure ()) (rangeAnti 0 $ quot len 2)
    -- | Sort
    foldr (\ i k -> do
        a0 <- readPrimArraySlice wsa 0
        a <- readPrimArraySlice wsa i
        writePrimArraySlice wsa i a0
        heapifyH i k a 0
      ) (pure ()) (rangeAnti 1 len)

-- do WP.sort . WP.MVector @_ @a i0 (i1 - i0) $ MutableByteArray wsa#

{- | Given a comparison function @cmp :: a -> a -> Ordering@,
    function @f :: a -> a@ with the property
    that for all @a :: a@, @cmp (f a) a = LT@ or @cmp (f a) a = EQ@ but
    there exists no @a' :: a@ such that @cmp (f a) a' = LT@ and @cmp a' a = LT@,
    and @ta :: MutablePrimArraySlice (PrimState m) a@ nondecreasing wrt @cmp@,
    maps @f@ over @ta@ while maintaining its nondecreasingness wrt @cmp@
    in a single pass using only constant auxiliary space.
-}
{-# INLINE qmMapSortedBy #-}
qmMapSortedBy :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    (a -> a -> Ordering) -> (a -> a) -> MutablePrimArraySlice (PrimState m) a -> m ()
qmMapSortedBy = \ cmp f wsa -> let len = sizeOfPrimArraySlice wsa in
    case compare 0 len of
        LT -> do
            a0 <- readPrimArraySlice wsa 0
            let a0' = f a0
            writePrimArraySlice wsa 0 a0'
            foldr (\ i k a'S iS -> do
                a <- readPrimArraySlice wsa i
                let a' = f a
                case cmp a'S a' of
                    GT -> do
                        writePrimArraySlice wsa iS a'
                        writePrimArraySlice wsa i a'S
                        k a'S (iS + 1)
                    EQ -> do
                        writePrimArraySlice wsa i a'
                        k a'S iS
                    LT -> do
                        writePrimArraySlice wsa i a'
                        k a' i
              ) (\ _ _ -> pure ()) (range 1 len) a0' 0
        _  -> pure ()

{-  The first @Just b@ returned by the predicate given as the first argument
    on the 'MutablePrimArraySlice' given as the second argument if one exists,
    else 'Nothing'\; for the standard 'PrimMonad' instances (i.e., 'IO'
    and 'Control.Monad.ST.ST') it is effectively an early-returning strict left fold
    in the 'GHC.Exts.State#' token.
-}
{-# INLINE findIndex #-}
findIndex :: forall (m :: Type -> Type) a b.
    (PrimMonad m, Prim a) =>
    (Int -> a -> Maybe b) -> MutablePrimArraySlice (PrimState m) a -> m (Maybe b)
findIndex = \ p wsa -> let len = sizeOfPrimArraySlice wsa in
    foldr (\ i k -> do
        a <- readPrimArraySlice wsa i
        case p i a of
            mb@(Just _) -> pure mb
            Nothing     -> k
      ) (pure Nothing) (range 0 len)

{- | Unsafely (i.e., without copying the backing memory)
   freezes a 'MutablePrimArraySlice' to a fold/build
   fusible 'Data.List.List'
-}
{-# INLINE unsafeFreezeToList #-}
unsafeFreezeToList :: forall (m :: Type -> Type) a.
    (PrimMonad m, Prim a) =>
    MutablePrimArraySlice (PrimState m) a -> m [a]
unsafeFreezeToList = \ (MutablePrimArraySlice wsa i0 i1) -> do
    va <- unsafeFreezePrimArray wsa
    pure $ List.build $ \ g b ->
        foldr (g . indexPrimArray va) b (range i0 i1)