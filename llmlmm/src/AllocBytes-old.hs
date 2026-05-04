{-# LANGUAGE Haskell2010
  , MagicHash
  , PatternSynonyms
  , ScopedTypeVariables
  , TupleSections
  , UnboxedTuples
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
  , Any
  , State#
  , RealWorld
  , Addr#
  , Ptr
      ( Ptr )
  , Int#
  , Word64#
  )

import GHC.IO -- /Why TF was this not somewhere more stable?/
  ( pattern IO )

import GHC.Word
  ( pattern W64# )

import Foreign.C.Types
  ( CSize 
      ( CSize )
  )


-- * Utils

{-# INLINE intToWord64Unsafe# #-}
intToWord64Unsafe# :: Int# -> Word64#
intToWord64Unsafe# = unsafeCoerce#

{-# INLINE stateToRealWorldUnsafe# #-}
stateToRealWorldUnsafe# :: forall s.
    State# s -> State# RealWorld
stateToRealWorldUnsafe# = unsafeCoerce#

{-# INLINE realWorldToStateUnsafe# #-}
realWorldToStateUnsafe# :: forall s.
    State# RealWorld -> State# s
realWorldToStateUnsafe# = unsafeCoerce#


-- * (A/Rea/Dea)llocation of foreign (non-GC) memory

foreign import ccall unsafe "stdlib.h malloc" c_malloc :: CSize -> IO (Ptr Any)

{-# INLINE mallocBytes# #-}
mallocBytes# :: forall s.
    Int# -> State# s -> (# State# s, Addr# #)
mallocBytes# = \ n -> case c_malloc (CSize $ W64# $ intToWord64Unsafe# n) of
    IO xsd -> \ s ->
        case xsd $ stateToRealWorldUnsafe# s of
            (# s', Ptr d #) -> (# , d #) $ realWorldToStateUnsafe# s'

foreign import ccall unsafe "stdlib.h calloc" c_calloc :: CSize -> CSize -> IO (Ptr Any)

{-# INLINE callocBytes# #-}
callocBytes# :: forall s.
    Int# -> State# s -> (# State# s, Addr# #)
callocBytes# = \ n -> case c_calloc (CSize $ W64# $ intToWord64Unsafe# n) 1 of
    IO xsd -> \ s ->
        case xsd $ stateToRealWorldUnsafe# s of
            (# s', Ptr d #) -> (# , d #) $ realWorldToStateUnsafe# s'

foreign import ccall unsafe "stdlib.h realloc" c_realloc :: Ptr Any -> CSize -> IO (Ptr Any)

{-# INLINE reallocBytes# #-}
reallocBytes# :: forall s.
    Addr# -> Int# -> State# s -> (# State# s, Addr# #)
reallocBytes# = \ d n -> case c_realloc (Ptr d) (CSize $ W64# $ intToWord64Unsafe# n) of
    IO xsd -> \ s ->
        case xsd $ stateToRealWorldUnsafe# s of
            (# s', Ptr d' #) -> (# , d' #) $ realWorldToStateUnsafe# s'

foreign import ccall unsafe "stdlib.h free" c_free :: Ptr Any -> IO ()

{-# INLINE free# #-}
free# :: forall s.
    Addr# -> State# s -> State# s
free# = \ d -> case c_free (Ptr d) of
    IO xsd -> \ s ->
        case xsd $ stateToRealWorldUnsafe# s of
            (# s', _ #) -> realWorldToStateUnsafe# s'