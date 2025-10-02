{-# LANGUAGE Haskell2010
  , GHCForeignImportPrim
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TupleSections
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
  ( unsafeCoerce#
  , State#
  , Addr#
  , Int#
  , Word#
  )


-- * (A/Rea/Dea)llocation of foreign (non-GC) memory

foreign import prim "mallocPrimOp"
    mallocBytesPrimOp# :: forall s.
        Int# -> State# s -> (# State#, Addr# #)

{-# INLINE mallocBytes# #-}
mallocBytes# :: forall s.
    Int# -> State# s -> (# State# s, Addr# #)
mallocBytes# = \ n s -> case mallocBytesPrimOp# (int2Word# n) s of
    d -> (# s, d #)

foreign import prim "callocPrimOp"
    callocBytesPrimOp# :: forall s.
        Word# -> Word# -> State# s -> Addr#

{-# INLINE callocBytes# #-}
callocBytes# :: forall s.
    Int# -> State# s -> (# State# s, Addr# #)
callocBytes# = \ n s -> case callocBytesPrimOp# (int2Word# n) 1## s of
    d -> (# s, d #)

foreign import prim "reallocPrimOp"
    reallocBytesPrimOp# :: forall s.
        Addr# -> Word# -> State# s -> Addr#

{-# INLINE reallocBytes# #-}
reallocBytes# :: forall s.
    Addr# -> Int# -> State# s -> (# State# s, Addr# #)
reallocBytes# = \ d n s -> case reallocBytesPrimOp# d (int2Word# n) s of
    d' -> (# s, d' #)

foreign import prim "freePrimOp"
    freePrimOp# :: forall s.
        Addr# -> State# s -> (# #)

{-# INLINE free# #-}
free# :: forall s.
    Addr# -> State# s -> State# s
free# = \ d s -> case freePrimOp# d s of
    (# #) -> s