{-# LANGUAGE Haskell2010
  , DataKinds
  , GHCForeignImportPrim
  , InstanceSigs
  , LinearTypes
  , MagicHash
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , RankNTypes
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , UnboxedTuples
  , UnliftedFFITypes
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Low-level linear 'Control.Monad.ST.runST' -}
module Data.State.Linear
  ( -- * ???
    Nat
      ( Z
      , S
      )
  , Mutability
      ( M
      , F
      )
  , Stemma
      ( R
      , H
      , V
      )
  , Heap
      ( GC
      , L
      )
  , State#
    -- * Low-level linear 'Control.Monad.ST.runST'
{-, runST0#
  , runST1#
  , runST2#
  , runST3#
  , runST4#
  , runST5#
  , runST6#
  , runST7#
  , runST8#-}
  ) where


-- + Imports

-- ++ From base >= 4.21 && < 4.23

import GHC.Exts
  ( TYPE
  , pattern One
  )

import Data.Coerce
  ( coerce )

import Unsafe.Coerce
  ( pattern UnsafeRefl
  , unsafeEqualityProof
  )

-- ++ (internal)

import Data.State.Linear.TH
  ( Nat
      ( Z
      , S
      )
  , Mutability
      ( M
      , F
      )
  , Stemma
      ( R
      , H
      , V
      )
  , Heap
      ( GC
      , L
      )
  , State#
  )


-- * Low-level linear 'Control.Monad.ST.runST'

-- runSTn#
