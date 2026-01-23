{-# LANGUAGE Haskell2010
  , CPP
  , DerivingStrategies
  , GADTSyntax
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , PackageImports
  , ScopedTypeVariables
  , StandaloneDeriving
#-}

{-# OPTIONS_GHC -Wall #-}

{- | 'ValStatus' represents the status of a \"value\"
    in the algorithm 'Match.match' in "Match"\;
    this module exports it, its constructors ('Dead' or 'Alive'),
    its 'Eq', 'Show', 'Ord', and the type 'ValStatusChunk'
    which allows 'ValStatus' values to be marshalled as
    the bits of a machine word for efficient packing/
    sequential access from/to (mutable) bytearrays
-}
module Data.Match.ValStatus
  ( module Data.Tuple
    -- * 'ValStatus'
  , ValStatus
      ( Dead
      , Alive
      )
    -- * 'ValStatusChunk'
  , ValStatusChunk
      ( ValStatusChunk
      , unValStatusChunk
      )
  , valStatusChunkSize
  , valStatusChunkInit
  , updateValStatusChunk
  , valStatusChunkToList
  , unValStatusChunkList
  ) where


-- + Imports

-- ++ base:

#include "MachDeps.h"

import Data.Bits
  ( (.&.)
  , (.|.)
  , complement
  , unsafeShiftL
  )

import qualified GHC.List as List
  ( build )


-- ++ primitive:

import Data.Primitive.Types
  ( Prim )


-- ++ From strict:

import "tsftl-lab-gauss" Data.Tuple
  ( Tup2 (..) )


-- * 'ValStatus'

{- | Type representing the status of a \"val\" in the algorithm 'Match.match' in "Match" -}
data ValStatus where
    Dead, Alive :: ValStatus

deriving stock instance Eq ValStatus
deriving stock instance Show ValStatus
deriving stock instance Ord ValStatus


-- * 'ValStatusChunk'

{- | Because in our use case we marshall our 'ValStatus' values
    into a 'Data.Primitive.PrimArray' from which asymptotically
    100% of our reads are sequential (i.e., streaming), it is
    efficient to represent said 'ValStatus' values as bits of
    'Word'-sized chunks
-}
newtype ValStatusChunk where
    ValStatusChunk :: {
        unValStatusChunk :: Word } ->
        ValStatusChunk

deriving newtype instance Prim ValStatusChunk
deriving stock instance Eq ValStatusChunk
deriving stock instance Show ValStatusChunk

{- | Word size in bits (platform-dependent)\;
    Presumably not in "GHC.Exts" only because of
    the lack of unlifted top-level bindings
-}
{-# INLINE valStatusChunkSize #-}
valStatusChunkSize :: Int
valStatusChunkSize = WORD_SIZE_IN_BITS

{- | 'ValStatusChunk' with first argument many
    initial bits set (overflows silently)
-}
{-# INLINE valStatusChunkInit #-}
valStatusChunkInit :: Int -> ValStatusChunk
valStatusChunkInit = \ i ->
    ValStatusChunk $ unsafeShiftL 1 i - 1

{- | Sets 'ValStatus' at index given as first argument
    to 'ValStatus' given as second argument
    in 'ValStatusChunk' given as third argument
-}
{-# INLINE updateValStatusChunk #-}
updateValStatusChunk :: Int -> ValStatus -> ValStatusChunk -> ValStatusChunk
updateValStatusChunk = \cases
    i Dead  (ValStatusChunk a) -> ValStatusChunk $ complement (unsafeShiftL 1 i) .&. a
    i Alive (ValStatusChunk a) -> ValStatusChunk $ unsafeShiftL 1 i .|. a

{- | Unfolds 'ValStatusChunk' to fold/build fusible
    indexed 'Data.List' of statically known length\;
    naively we should be able to speed this up
    by a factor of about \(n \leq 64\)
    at the expense of bloating code by a factor
    of about \(2^{n}\)
-}
{-# INLINE valStatusChunkToList #-}
valStatusChunkToList :: ValStatusChunk -> [Tup2 Int ValStatus]
valStatusChunkToList = \ (ValStatusChunk a) ->
    List.build $ \ g b ->
        foldr (\ i -> case unsafeShiftL 1 i .&. a of
            0 -> g $ Tup2 i Dead
            _ -> g $ Tup2 i Alive
          ) b [0 .. valStatusChunkSize - 1]

{- | Unfolds 'Data.List.List' of 'ValStatusChunk's
    to fold/build fusible 'Data.List.List' of 'ValStatus'es
-}
{-# INLINE unValStatusChunkList #-}
unValStatusChunkList :: [ValStatusChunk] -> [ValStatus]
unValStatusChunkList = \ sa ->
    List.build $ \ g b ->
        foldr (\ a b' -> foldr (g . of2entry1) b' $ valStatusChunkToList a) b sa