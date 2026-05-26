{-# LANGUAGE Haskell2010
  , MagicHash
  , PatternSynonyms
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Exposes constructor of 'Data.State.Linear.LAlloc#' -}
module Data.State.Linear.Unsafe
  ( -- * Constructor of 'Data.State.Linear.LAlloc#'
    pattern LAlloc#
  ) where

import Data.State.Linear.TH
  ( pattern LAlloc# )