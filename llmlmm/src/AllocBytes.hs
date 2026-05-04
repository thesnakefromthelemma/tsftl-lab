{-# LANGUAGE Haskell2010
  , GHCForeignImportPrim
  , MagicHash
  , ScopedTypeVariables
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | (A/Rea/Dea)llocation of foreign (non-GC) memory -}
module AllocBytes
  ( -- * (A/Rea/Dea)llocation of foreign (non-GC) memory
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


-- ++ (internal)

import Data.Addr
  ( Addr# 
    ( Addr# )
  )


-- * (A/Rea/Dea)llocation of foreign (non-GC) memory

{- | Given argument @n@,
    returns the 'State#' action allocating @n@ bytes on the foreign heap,
    its result the machine address of the allocation
-}
foreign import prim "mallocPrimOp"
    mallocBytes# :: forall s. Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @n@, @k@,
    returns the 'State#' action allocating @n@ zeroed objects of size @k@ bytes on the foreign heap,
    its result the machine address of the allocation
-}
foreign import prim "callocPrimOp"
    callocBytes# :: forall s. Int# -> Int# -> State# s -> (# State# s, Addr# s #)

{- | Given arguments @p@, @n@,
    returns the 'State#' action resizing @p@\'s allocation to @n@ bytes,
    its result the machine address of the resized allocation
-}
foreign import prim "reallocPrimOp"
    reallocBytes# :: forall s. Addr# s -> Int# -> State# s -> (# State# s, Addr# s #)

{- | Given argument @p@,
    returns the 'State#' action freeing @p@\'s allocation
-}
foreign import prim "freePrimOp"
    free# :: forall s. Addr# s -> State# s -> State# s