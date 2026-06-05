{-# LANGUAGE Haskell2010
  , MagicHash
  , PatternSynonyms
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Exposes constructor of 'Data.State.Linear.State#' -}
module Data.State.Linear.Unsafe
  ( -- * Constructor of 'Data.State.Linear.State#'
    pattern State#
  ) where

import Data.State.Linear.TH
  ( pattern State# )