{-# LANGUAGE Haskell2010
  , MagicHash
  , PatternSynonyms
#-}

{-# OPTIONS_GHC -Wall #-}

{- | Exposes constructor of 'Data.Addr.Linear.Addr#' -}
module Data.Addr.Linear.Unsafe
  ( -- * Constructor of 'Data.Addr.Linear.Addr#'
    pattern Addr#
  ) where

import Data.Addr.Linear.TH
  ( pattern Addr# )