{-# LANGUAGE Haskell2010
  , GHCForeignImportPrim
  , LinearTypes
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

-- + From base >= 4.21 && << 4.22

import GHC.Exts
  ( State#
  , Addr#
  , Int#
  )


-- * (A/Rea/Dea)llocation of foreign (non-GC) memory

foreign import prim "mallocPrimOp"
    mallocBytes# :: forall s.
        Int# -> State# s -> (# State# s, Addr# #)

foreign import prim "callocPrimOp"
    callocBytes# :: forall s.
        Int# -> Int# -> State# s -> (# State# s, Addr# #)

foreign import prim "reallocPrimOp"
    reallocBytes# :: forall s.
        Addr# -> Int# -> State# s -> (# State# s, Addr# #)

foreign import prim "freePrimOp"
    free# :: forall s.
        Addr# -> State# s -> State# s