{-# LANGUAGE Haskell2010
  , MagicHash
  , PatternSynonyms
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Exposes constructor of 'Data.State.Linear.Alloc#' -}
module Data.State.Linear.Unsafe
  ( -- * Constructor of 'Data.State.Linear.Alloc#'
    pattern Alloc#
  ) where

import Data.State.Linear.TH
  ( pattern Alloc# )