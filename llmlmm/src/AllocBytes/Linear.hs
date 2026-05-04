{-# LANGUAGE Haskell2010
  , GHCForeignImportPrim
  , LinearTypes
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TypeApplications
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-# OPTIONS_GHC -Wall -Wno-overlapping-patterns -Wno-inaccessible-code #-}

{- | Linear (a/rea/dea)llocation of foreign (non-GC) memory -}
module AllocBytes.Linear
  ( -- * Linear (a/rea/dea)llocation of foreign (non-GC) memory
    mallocBytes#
  , callocBytes#
  , reallocBytes#
  , free#
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( State#
  , Int#
  )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )


-- ++ (internal)

import Data.Addr.Linear
  ( Addr#
      ( Addr# )
  )


-- * (A/Rea/Dea)llocation of foreign (non-GC) memory

-- Cf. #18472 as to why the coercions are necessary.

foreign import prim "mallocPrimOp"
    mallocBytes_primOp# :: forall s. Int# -> State# s -> Addr# s

{- | Given argument @n@,
    returns the linear 'State#' action allocating @n@ bytes on the foreign heap,
    its result the machine address of the allocation
-}
{-# INLINE mallocBytes# #-}
mallocBytes# :: forall s. Int# -> State# s %1 -> Addr# s
mallocBytes# = case unsafeEqualityProof @(Int# -> State# s -> Addr# s) @(Int# -> State# s %1 -> Addr# s) of
    UnsafeRefl -> mallocBytes_primOp#

foreign import prim "callocPrimOp"
    callocBytes_primOp# :: forall s. Int# -> Int# -> State# s -> Addr# s

{- | Given arguments @n@, @k@,
    returns the linear 'State#' action allocating @n@ zeroed objects of size @k@ bytes on the foreign heap,
    its result the machine address of the allocation
-}
{-# INLINE callocBytes# #-}
callocBytes# :: forall s. Int# -> Int# -> State# s %1 -> Addr# s
callocBytes# = case unsafeEqualityProof @(Int# -> Int# -> State# s -> Addr# s) @(Int# -> Int# -> State# s %1 -> Addr# s) of
    UnsafeRefl -> callocBytes_primOp#

foreign import prim "reallocPrimOp"
    reallocBytes_primOp# :: forall s. Addr# s -> Int# -> Addr# s

{- | Given arguments @p@, @n@,
    linearly consumes @p@, resizing @p@\'s allocation to @n@ bytes,
    returning the machine address of the resized allocation
-}
{-# INLINE reallocBytes# #-}
reallocBytes# :: forall s. Addr# s %1 ->  Int# -> Addr# s
reallocBytes# = case unsafeEqualityProof @(Addr# s -> Int# -> Addr# s) @(Addr# s %1 -> Int# -> Addr# s) of
    UnsafeRefl -> reallocBytes_primOp#

foreign import prim "freePrimOp"
    free_primOp# :: forall s. Addr# s -> (# #)

{- | Given argument @p@,
    linearly consumes @p@,
    returning @(# #)@
-}
{-# INLINE free# #-}
free# :: forall s. Addr# s %1 -> (# #)
free# = case unsafeEqualityProof @(Addr# s -> (# #)) @(Addr# s %1 -> (# #)) of
    UnsafeRefl -> free_primOp#