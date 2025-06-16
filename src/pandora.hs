{-# OPTIONS_GHC
    -fllvm
    -funbox-strict-fields
    -O2
    -Wall
    #-}

{-# LANGUAGE
      Haskell2010
    , BangPatterns
    , GADTs
    , KindSignatures
    , ScopedTypeVariables
    #-}

module Pandora where


-- IMPORTS --

import Prelude hiding
    ( head
    , tail
    , take
    )

import Data.Kind
    ( Type )

import qualified Data.Vector.Strict as VS
    ( Vector
    , empty
    , cons
    , take
    , length
    )

import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )

import System.IO.Unsafe
   ( unsafePerformIO )


-- STREAM --

data Stream :: Type -> Type where
    Stream :: forall a. a -> Stream a -> Stream a

head :: forall a. Stream a -> a
head = \ ~(Stream a _) -> a

tail :: forall a. Stream a -> Stream a
tail = \ ~(Stream _ sa) -> sa

cut :: forall a. Int -> Stream a -> (VS.Vector a, Stream a)
cut = undefined


-- PANDORA --

data Pandora :: Type -> Type where
    Pandora :: forall a. IORef (VS.Vector a) -> IORef (Stream a) -> Pandora a

{-# NOINLINE take #-}
take :: forall a. Int -> Pandora a -> VS.Vector a
take = \n -> \ ~(Pandora rva sa) -> unsafePerformIO $ do
    va <- readIORef rva
    len <- length va
    case compare n len of
        GT -> do
            let va' :: VS.Vector a; sa' :: Stream a
                (va', sa') = cut (n - len) sa
            writeIORef rva (va <> va')
            pure 
        _  -> VS.take n va
